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
import           Control.Exception          (Exception (..), SomeException, toException)

import           Control.Lens
import           Control.Monad.Free
import           Control.Monad (forever)
import qualified Control.Monad.Trans.Free    as FT
-- import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader.Class (MonadReader (..))
-- import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Either (EitherT (..), left, eitherT)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
-- import           Data.Aeson                 (FromJSON (..), ToJSON (..))
-- import qualified Data.Aeson                 as Aeson
import           Data.Data                  (Typeable)
import           Data.Text                  (Text)
-- import qualified Data.Text                  as T
-- import qualified Data.Text.Encoding         as T
-- import           Data.UUID                  (UUID)
-- import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID
import           Database.Redis             (Redis)
import qualified Database.Redis             as Redis
import           GHC.Generics
import Pipes
import           Cdo.Query
import           Cdo.Types
import Cdo.Write


newtype Query a = Query { unQuery :: (Redis a) } deriving (Functor, Applicative, Monad)

runQuery :: MonadIO m => Redis a -> CommandMonad err evt m a
runQuery q = do
  rCon <- view conn <$> ask
  liftIO $ Redis.runRedis rCon q

redisQuery
  :: MonadIO m
  => Redis (Either Redis.Reply a)
  -> (a -> CommandMonad SomeException evt m b)
  -> CommandMonad SomeException evt m b
redisQuery q f = eitherT queryError f (EitherT $ runQuery q)

---------------------------------------------------------------------------------
yieldEvent :: Monad m => evt -> CommandMonad err evt m ()
yieldEvent evt = CommandMonad . lift . lift $ yield evt

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

newtype CommandMonad err evt m a = CommandMonad {
    unCommand :: EitherT (CommandError err) (ReaderT Env (Producer evt m)) a
  } deriving (Functor, Applicative, Monad, MonadIO)

runCommandMonad
  :: CommandMonad e evt m a
  -> Env
  -> Producer evt m (Either (CommandError e) a)
runCommandMonad = runReaderT . runEitherT . unCommand

instance MonadTrans (CommandMonad err evt) where
  lift = CommandMonad . lift . lift . lift

instance (Monad m) => MonadReader Env (CommandMonad err evt m) where
  ask     = CommandMonad (lift ask)
  local f = CommandMonad . local f . unCommand

instance (Monad m) => Alternative (CommandMonad err evt m) where
  empty = CommandMonad . left $ CommandError "empty"
  CommandMonad (EitherT m1) <|> CommandMonad (EitherT m2) =
     CommandMonad $ EitherT $ m1 >>= either (\_ -> m2) (return . Right)

---------------------------------------------------------------------------------
data EventError e =
    EventError Text
  | WriteError e
  deriving (Show, Typeable, Generic)

newtype EventMonad err evt m a = EventMonad {
    unEvent :: EitherT (EventError err) (ReaderT Env (Consumer evt m)) a
  } deriving (Functor, Applicative, Monad, MonadIO)

runEventMonad
  :: EventMonad e evt m a
  -> Env
  -> Consumer evt m (Either (EventError e) a)
runEventMonad = runReaderT . runEitherT . unEvent

instance MonadTrans (EventMonad err evt) where
  lift = EventMonad . lift . lift . lift

instance (Monad m) => MonadReader Env (EventMonad err evt m) where
  ask     = EventMonad (lift ask)
  local f = EventMonad . local f . unEvent

instance (Monad m) => Alternative (EventMonad err evt m) where
  empty = EventMonad . left $ EventError "empty"
  EventMonad (EitherT m1) <|> EventMonad (EitherT m2) =
     EventMonad $ EitherT $ m1 >>= either (\_ -> m2) (return . Right)


---------------------------------------------------------------------------------
businessLogicError :: Monad m => err -> CommandMonad err evt m a
businessLogicError = CommandMonad . left . BusinessLogicError

queryError :: Monad m => Redis.Reply -> CommandMonad SomeException evt m a
queryError r = CommandMonad . left . QueryError . toException $ QueryException err
  where err = case r of
                Redis.Error e -> e
                _             -> "Something went wrong with redis"

writeError :: Monad m => Redis.Reply -> EventMonad SomeException evt m a
writeError r = EventMonad . left . WriteError . toException $ QueryException err
  where err = case r of
                Redis.Error e -> e
                _             -> "Something went wrong with redis"


---------------------------------------------------------------------------------
runSingle :: forall a m. (MonadIO m) => CommandF a -> CommandMonad SomeException Event m a
runSingle = go
  where
    go :: CommandF a -> CommandMonad SomeException Event m a
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
applyEvent :: Event -> CommandMonad SomeException LoggedEvent m ()
applyEvent evt = undefined


---------------------------------------------------------------------------------
type CMD m = CommandMonad SomeException Event m

run
  :: forall a m .(Functor m, Applicative m, MonadIO m)
  => Env
  -> FT.FreeT CommandF (CMD m) a
  -> m (Either (CommandError SomeException) ())
run env@(Env con) ftc = do
  let evts = runCommandMonad (step ftc) env
  runEffect ((void <$> evts) >-> writeEvents con)
  where
    step :: FT.FreeT CommandF (CMD m) b -> CommandMonad SomeException Event m b
    step cmd' = do
      cmd <- FT.runFreeT cmd'
      case cmd of
        FT.Pure a -> return a
        FT.Free c -> do
          step =<< runSingle c

writeEvents :: MonadIO m => Redis.Connection -> Consumer Event m (Either (CommandError SomeException) ())
writeEvents con = forever $ do
  evt <- await
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

