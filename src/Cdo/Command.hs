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
-- import           Control.Monad (forever)
import qualified Control.Monad.Trans.Free   as FT
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
-- import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader.Class (MonadReader (..))
-- import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.RWS.Strict   (MonadReader (..), MonadState (..),
                                             MonadWriter (..), RWST (..), tell)
import           Control.Monad.Trans.Either (EitherT (..), eitherT, hoistEither,
                                             left)
-- import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import           Data.Data                  (Typeable)
import qualified Data.Sequence              as Seq
import           Data.Text                  (Text)
-- import qualified Data.Text                  as T
-- import qualified Data.Text.Encoding         as T
-- import           Data.UUID                  (UUID)
-- import qualified Data.UUID                  as UUID
import           Cdo.Query
import           Cdo.Types
import           Cdo.Write
import qualified Data.UUID.V4               as UUID
import           Database.Redis             (Redis)
import qualified Database.Redis             as Redis
import           GHC.Generics
import           Pipes


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
yieldEvent :: Monad m => evt -> CommandT err evt m ()
yieldEvent evt = CommandT $ lift  $ tell $ Seq.singleton evt

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
     CommandT $ EitherT $ m1 >>= either (\_ -> m2) (return . Right)

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
applyEvent :: forall m. (MonadIO m) => Redis.Connection -> Event -> m (CmdResult ())
applyEvent con evt = do
  case evt of
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


---------------------------------------------------------------------------------
type FreeCMD m = FT.FreeT CommandF (CommandT SomeException Event m)

run
  :: forall a m .(Functor m, Applicative m, MonadIO m)
  => Env
  -> FreeCMD m a
  -> m (CmdResult ())
run env@(Env con) ftc = do
  (res, _, evts) <- runCommandT (step ftc) env ()
  case res of
    Left err -> do
      liftIO $ print err
      return $ Left err
    Right _ -> return $ Right ()
  --runEffect $ for cmdRes $ \a -> lift $ print a
  -- runEffect ((void <$> evts) >-> writeEvents con)
  where
    step :: (MonadIO m) => FreeCMD m a -> CommandT SomeException evt m a
    step (FT.FreeT cmd') = do
      cmd <- runProducer cmd' undefined -- (const $ return ())
      case cmd of
        FT.Pure a -> return a
        FT.Free c -> do
          let sngl = runSingle c
          step =<< runProducer sngl (applyAndLog con)

applyAndLog :: MonadIO m => Redis.Connection -> Event -> EventMonad m (CmdResult a)
applyAndLog evt = do
  
  lift $ do
    applyEvent con evt
    logEvent con (LoggedEvent () evt)

type EventMonad = ReaderT Redis.Connection

logEvent :: MonadIO m => Redis.Connection -> LoggedEvent -> m (CmdResult ())
logEvent con evt =
  runEitherT $ eitherT
    writeError'
    (const $ return ())
    (EitherT $ liftIO $  Redis.runRedis con $ writeEvent evt)
  where
    writeError' r = left . QueryError . toException $ QueryException err
      where err = case r of
                Redis.Error e -> e
                _             -> "Something went wrong with redis"

mainM :: MonadFree CommandF m => m ()
mainM = do
  acc <- openAccount "Ben"
  creditAccount (acc ^. accId) 100

main :: IO ()
main = do
  env <- Env <$> Redis.connect Redis.defaultConnectInfo
  print =<< run env mainM

runProducer :: (MonadIO m) => CommandT SomeException evt m a -> Consumer evt m (CmdResult a) -> CommandT SomeException evt m a
runProducer m eff = do
  env <- ask
  let prod = runCommandT m env
  res <- lift $ runEffect $ prod >-> eff
  CommandT $ hoistEither res
