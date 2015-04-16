{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Cdo.Command where
import           Control.Applicative
import           Control.Exception          (Exception (..), SomeException,
                                             toException)
import           Control.Lens
import           Control.Monad.Free
import Control.Monad.Morph as M
import           Data.Foldable              (forM_)
import           Data.Traversable           (forM, sequence)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader.Class (MonadReader (..))
import           Control.Monad.RWS.Strict   (MonadReader (..), MonadState (..),
                                             MonadWriter (..), RWST (..), tell)
-- import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Either (EitherT (..), eitherT, hoistEither,
                                             left)
import qualified Control.Monad.Trans.Free   as FT
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Control.Monad.Writer.Strict (WriterT, runWriterT)
import qualified Control.Monad.Writer.Strict as W
import           Data.Data                  (Typeable)
import qualified Data.Sequence              as Seq
import           Data.Text                  (Text)
import           Cdo.Query
import           Cdo.Types
import           Cdo.Write
import qualified Data.UUID.V4               as UUID
import           Database.Redis             (Redis)
import qualified Database.Redis             as Redis
import           GHC.Generics
-- import Pipes (Consumer, runEffect, (>->))
import           Prelude                    hiding (sequence)
-- import qualified Pipes as Pipes



newtype Query a = Query { unQuery :: (Redis a) } deriving (Functor, Applicative, Monad)

type CmdResult = Either (CommandError SomeException)

runQuery :: MonadIO m => Redis a -> CommandT err evt m a
runQuery q = do
  rCon <- view conn <$> ask
  liftIO $ Redis.runRedis rCon q

redisQuery
  :: MonadIO m
  => Redis (Either Redis.Reply a)
  -> (a -> CommandT SomeException evt m b)
  -> CommandT SomeException evt m b
redisQuery q f = eitherT queryError f (EitherT $ runQuery q)

---------------------------------------------------------------------------------
yieldEvent :: (Show evt, MonadIO m) => evt -> CommandT err evt m ()
yieldEvent evt = CommandT $ lift $ tell $ Seq.singleton evt

openAccount :: MonadFree CommandF m => AccountName -> m Account
openAccount acc = liftF $! Open acc id

closeAccount :: MonadFree CommandF m => AccountId -> m ()
closeAccount aid = liftF $! Close aid ()

debitAccount :: MonadFree CommandF m => AccountId -> Double -> m ()
debitAccount aid n = liftF $! Debit aid n ()

creditAccount :: MonadFree CommandF m => AccountId -> Double -> m ()
creditAccount aid n = liftF $! Credit aid n ()

---------------------------------------------------------------------------------
data CommandError e =
    AccountError Text
  | CommandError Text
  | QueryError e
  | BusinessLogicError e
  deriving (Show, Typeable, Generic)

data EventError e =
    EventError Text
  | WriteError e
  deriving (Show, Typeable, Generic)


newtype CommandT err evt m a = CommandT {
    unCommand :: EitherT (CommandError err) (RWST Env (Seq.Seq evt) () m) a
  } deriving (Functor, Applicative, Monad, MonadIO)


runCommandT
  :: CommandT err evt m a
  -> Env
  -> ()
  -> m (Either (CommandError err) a, (), Seq.Seq evt)
runCommandT = runRWST . runEitherT . unCommand

instance MonadTrans (CommandT err evt) where
  lift = CommandT . lift . lift

-- instance (Monad m) => MonadState CState (CommanderT e m) where
--   get = CommanderT (lift get)
--   put = CommanderT . lift . put

instance (Monad m) => MonadReader Env (CommandT err evt m) where
  ask     = CommandT (lift ask)
  local f = CommandT . local f . unCommand

instance (Monad m) => Alternative (CommandT err evt m) where
  empty = CommandT . left $ CommandError "empty"
  CommandT (EitherT m1) <|> CommandT (EitherT m2) =
     CommandT $ EitherT $ m1 >>= either (const m2) (return . Right)

instance (M.MFunctor (CommandT err evt)) where
  hoist nat m = CommandT . EitherT $ RWST (\r s -> nat (runCommandT m r s))
---------------------------------------------------------------------------------
businessLogicError :: Monad m => err -> CommandT err evt m a
businessLogicError = CommandT . left . BusinessLogicError

queryError :: Monad m => Redis.Reply -> CommandT SomeException evt m a
queryError r = CommandT . left . QueryError . toException $ QueryException err
  where err = case r of
                Redis.Error e -> e
                _             -> "Something went wrong with redis"

writeError :: Monad m => Redis.Reply -> CommandT SomeException evt m a
writeError r = CommandT . left . QueryError . toException $ QueryException err
  where err = case r of
                Redis.Error e -> e
                _             -> "Something went wrong with redis"


---------------------------------------------------------------------------------
runSingle :: forall a m. (MonadIO m) => CommandF a -> CommandT SomeException Event m a
runSingle = go
  where
    go :: CommandF a -> CommandT SomeException Event m a
    go (Open name f) = redisQuery (accountExists name) $ \exists ->
      if exists
        then businessLogicError $ toException AccountAlreadyExists
        else do
          newId <- AccountId <$> liftIO UUID.nextRandom
          let acc = Account newId name (Amount 0)
          yieldEvent (AccountOpened acc)
          return $ f acc

    go (Credit aid amount nxt) = redisQuery (accountBalance aid) creditAccount'
      where
        creditAccount' Nothing = businessLogicError . toException $ AccountBalanceNotFound aid
        creditAccount' (Just _) = do
          yieldEvent (AccountCredited aid (Amount amount))
          return nxt

    go (Debit aid amount nxt) = redisQuery (accountBalance aid) debitAccount'
      where
        debitAccount' Nothing = businessLogicError . toException $ AccountBalanceNotFound aid
        debitAccount' (Just (Amount n)) =
          if n < amount
             then businessLogicError . toException $ AccountBalanceInsufficient
             else do
               yieldEvent (AccountDebited aid (Amount n))
               return nxt

    go (Close aid nxt) = redisQuery (accountBalance aid) closeAccount'
      where
        closeAccount' Nothing = businessLogicError . toException $ AccountBalanceNotFound aid
        closeAccount' (Just (Amount n)) =
          if n /= 0
             then businessLogicError . toException $ AccountStillHasBalance aid
             else do
               yieldEvent (AccountClosed aid)
               return nxt

---------------------------------------------------------------------------------
type FreeCMD m = FT.FreeT CommandF (CommandT SomeException Event m)

run
  :: forall a m .(Functor m, Applicative m, MonadIO m)
  => Env
  -> FreeCMD m a
  -> m (CmdResult (Seq.Seq LoggedEvent))
run env@(Env con) ftc = do
  ((ret, _, _), evts) <- runWriterT $ runCommandT (step ftc) env ()
  return $ const evts <$> ret
  where
    step :: FreeCMD m a -> CommandT SomeException Event (WriterT (Seq.Seq LoggedEvent) m) a
    step (FT.FreeT cmd') = do
      cmd <- M.hoist lift cmd'
      case cmd of
        FT.Pure a -> return a
        FT.Free c -> do
          liftIO $ print (cmdToRep c)
          (res, _, evts) <- liftIO $ runCommandT (runSingle c) env ()
          events <- liftIO $ runReaderT (applyAndLog evts) con
          case (,) <$> res <*> events of
            Left err -> CommandT $ left err
            Right (nxt, loggedEvents) -> do
              forM_ loggedEvents (lift . W.tell . Seq.singleton)
              step nxt


applyAndLog
  :: (MonadReader Redis.Connection m, MonadIO m, Functor m, Applicative m)
  => Seq.Seq Event
  -> m (CmdResult (Seq.Seq LoggedEvent))
applyAndLog evts = do
  con <- ask
  runEitherT $ forM evts $ \evt -> do
    applyEvent con evt
    let le = LoggedEvent () evt
    logEvent con le
    return le

applyEvent
  :: forall m. (MonadIO m)
  => Redis.Connection
  -> Event
  -> EitherT (CommandError SomeException) m ()
applyEvent con evt =
  EitherT $ case evt of
    AccountOpened acc -> apply (writeAccount acc)
    AccountCredited aid amt -> apply (saveAmount aid amt)
  where
    apply :: Redis (Either Redis.Reply a) -> m (CmdResult ())
    apply q =
       runEitherT $ eitherT
         writeError'
         (const $ return ())
         (EitherT $ liftIO $  Redis.runRedis con $ q)
       where
         writeError' r = left . QueryError . toException $ QueryException err
           where err = case r of
                     Redis.Error e -> e
                     _             -> "Something went wrong with redis"


logEvent
  :: MonadIO m
  => Redis.Connection
  -> LoggedEvent
  -> EitherT (CommandError SomeException) m ()
logEvent con evt =
  eitherT
    writeError'
    (const $ return ())
    (EitherT $ liftIO $  Redis.runRedis con $ writeEvent evt)
  where
    writeError' r = left . QueryError . toException $ QueryException err
      where err = case r of
                Redis.Error e -> e
                _             -> "Something went wrong with redis"

---------------------------------------------------------------------------------
mainM :: MonadFree CommandF m => m ()
mainM = do
  acc <- openAccount "Ben"
  creditAccount (acc ^. accId) 100

main :: IO ()
main = do
  env <- Env <$> Redis.connect Redis.defaultConnectInfo
  print =<< run env mainM
