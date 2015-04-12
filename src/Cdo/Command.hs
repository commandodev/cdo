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
-- import           Control.Monad (forever)
import qualified Control.Monad.Trans.Free    as FT
-- import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader.Class (MonadReader (..))
-- import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Either (EitherT (..), left, eitherT, hoistEither)
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

runQuery :: MonadIO m => Redis a -> CommandT err m a
runQuery q = do
  rCon <- view conn <$> ask
  liftIO $ Redis.runRedis rCon q

redisQuery
  :: MonadIO m
  => Redis (Either Redis.Reply a)
  -> (a -> CommandT SomeException m b)
  -> CommandT SomeException m b
redisQuery q f = eitherT queryError f (EitherT $ runQuery q)

---------------------------------------------------------------------------------
yieldEvent :: Monad m => evt -> CommandMonad err evt m ()
yieldEvent evt = CommandT . lift . lift $ yield evt

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


newtype CommandT err m a = CommandT {
    unCommand :: EitherT (CommandError err) (ReaderT Env m) a
  } deriving (Functor, Applicative, Monad, MonadIO)

type CommandMonad err evt m a = CommandT err (Producer evt m) a
type EventMonad err evt m a = CommandT err (Consumer evt m) a

runCommandT
  :: CommandT e m a
  -> Env
  -> m (Either (CommandError e) a)
runCommandT = runReaderT . runEitherT . unCommand

instance MonadTrans (CommandT err) where
  lift = CommandT . lift . lift

instance (Monad m) => MonadReader Env (CommandT err m) where
  ask     = CommandT (lift ask)
  local f = CommandT . local f . unCommand

instance (Monad m) => Alternative (CommandT err m) where
  empty = CommandT . left $ CommandError "empty"
  CommandT (EitherT m1) <|> CommandT (EitherT m2) =
     CommandT $ EitherT $ m1 >>= either (\_ -> m2) (return . Right)

---------------------------------------------------------------------------------
businessLogicError :: Monad m => err -> CommandT err m a
businessLogicError = CommandT . left . BusinessLogicError

queryError :: Monad m => Redis.Reply -> CommandT SomeException m a
queryError r = CommandT . left . QueryError . toException $ QueryException err
  where err = case r of
                Redis.Error e -> e
                _             -> "Something went wrong with redis"

writeError :: Monad m => Redis.Reply -> EventMonad SomeException evt m a
writeError r = CommandT . left . QueryError . toException $ QueryException err
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
applyEvent :: (MonadIO m) => Redis.Connection -> Event -> m (Either (CommandError SomeException) ())
applyEvent con evt = do
  liftIO $ putStrLn $ "APPLY: " ++ (show evt)
  return $ Right ()


---------------------------------------------------------------------------------
type FreeCMD m = FT.FreeT CommandF (CommandT SomeException (Producer Event m))

run
  :: forall a m .(Functor m, Applicative m, MonadIO m)
  => Env
  -> FreeCMD m a
  -> m (Either (CommandError SomeException) ())
run env@(Env con) ftc = do
  res <- runCommandT (step ftc) env
  case res of
    Left err -> do
      liftIO $ print err
      return $ Left err
    Right _ -> return $ Right ()
  --runEffect $ for cmdRes $ \a -> lift $ print a
  -- runEffect ((void <$> evts) >-> writeEvents con)
  where
    step :: (MonadIO m) => FreeCMD m a -> CommandT SomeException m a
    step (FT.FreeT cmd') = do
      cmd <- runProducer cmd' (const $ return ())
      case cmd of
        FT.Pure a -> return a
        FT.Free c -> do
          let sngl = runSingle c
          step =<< runProducer sngl (applyAndLog con)

applyAndLog :: MonadIO m => Redis.Connection -> Event -> m ()
applyAndLog con evt = do
  _ <- applyEvent con evt
  logEvent con (LoggedEvent () evt)
  return ()

logEvent :: MonadIO m => Redis.Connection -> LoggedEvent -> m (Either (CommandError SomeException) ())
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

runProducer :: (MonadIO m) => CommandMonad er evt m a -> (evt -> Effect m ()) -> CommandT er m a
runProducer m eff = do
  env <- ask
  let prod = runCommandT m env
  res <- lift $ runEffect $ for prod eff
  CommandT $ hoistEither res
