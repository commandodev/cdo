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
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader.Class (MonadReader (..))
import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Either (EitherT (..), left, right, eitherT)
import           Control.Monad.Trans.Reader (ReaderT, asks)
import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import qualified Data.Aeson                 as Aeson
import           Data.Data                  (Data, Typeable)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.UUID                  (UUID)
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID
import           Database.Redis             (Redis, RedisCtx)
import qualified Database.Redis             as Redis
import           GHC.Generics
import Pipes
import           Cdo.Query
import           Cdo.Types


runQuery :: MonadIO m => Redis a -> CommandMonad err evt m a
runQuery q = do
  rCon <- view conn <$> ask
  liftIO $ Redis.runRedis rCon q

yieldEvent :: Monad m => evt -> CommandMonad err evt m ()
yieldEvent evt = CommandMonad . lift . lift $ yield evt

openAccount :: MonadFree CommandF m => AccountName -> m Account
openAccount acc = liftF $! Open acc id

data CommandError e =
    AccountError Text
  | CommandError Text
  | QueryError e
  | BusinessLogicError e
  deriving (Show, Typeable, Generic)


newtype CommandMonad err evt m a = CommandMonad {
    unCommand :: EitherT (CommandError err) (ReaderT Env (Producer evt m)) a
  } deriving (Functor, Applicative, Monad, MonadIO)


instance MonadTrans (CommandMonad err evt) where
  lift = CommandMonad . lift . lift . lift

-- instance (Monad m) => MonadState CState (CommandMonad e m) where
--   get = CommandMonad (lift get)
--   put = CommandMonad . lift . put

instance (Monad m) => MonadReader Env (CommandMonad err evt m) where
  ask     = CommandMonad (lift ask)
  local f = CommandMonad . local f . unCommand

instance (Monad m) => Alternative (CommandMonad err evt m) where
  empty = CommandMonad . left $ CommandError "empty"
  CommandMonad (EitherT m1) <|> CommandMonad (EitherT m2) =
     CommandMonad $ EitherT $ m1 >>= either (\_ -> m2) (return . Right)

businessLogicError :: Monad m => err -> CommandMonad err evt m a
businessLogicError = CommandMonad . left . BusinessLogicError

queryError :: Monad m => Redis.Reply -> CommandMonad SomeException evt m a
queryError r = CommandMonad . left . QueryError . toException $ QueryException err
  where err = case r of
                Redis.Error e -> e
                _             -> "Something went wrong with redis"


runCommand :: forall a m. (MonadIO m) => CommandF a -> CommandMonad SomeException Event m a
runCommand = go
  where
    go :: CommandF a -> CommandMonad SomeException Event m a
    go (Open name f) = do
      eitherT queryError
        createAccount
        (EitherT $ runQuery $ accountExists name)
      where
        createAccount exists =
          if exists
            then businessLogicError $ toException AccountAlreadyExists
            else do
              newId <- AccountId <$> liftIO UUID.nextRandom
              let acc = Account newId name (Amount 0)
              yieldEvent (AccountOpened acc)
              return $ f acc
