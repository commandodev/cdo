{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Cdo.Types

       where
import           Control.Applicative
import           Control.Exception          (Exception (..))
import           Control.Lens
import           Control.Monad.Free
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Either (EitherT, left, right)
import           Control.Monad.Trans.Reader (ReaderT, asks)
import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import qualified Data.Aeson                 as Aeson
import Data.ByteString (ByteString)
import           Data.Data                  (Data, Typeable)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.UUID                  (UUID)
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID
import qualified Database.Redis             as Redis
import           GHC.Generics

type AccountName = Text

data Env = Env {
    _conn :: Redis.Connection
  }

makeLenses ''Env

data AccountException =
    AccountAlreadyExists
  | AccountBalanceInsufficient
  deriving (Show, Typeable)

instance Exception AccountException

data QueryException =
    QueryException ByteString
  deriving (Show, Typeable)

instance Exception QueryException

newtype AccountId = AccountId {
    unaccId :: UUID
  } deriving (Show, Eq, Ord, Data, Generic, Typeable)

makeWrapped ''AccountId

newtype Amount = Amount {
    unAmount :: Double
  } deriving (Show, Eq, Ord, Data, Generic, Typeable, ToJSON, FromJSON)

data Account = Account {
    _accId      :: AccountId
  , _accName    :: AccountName
  , _accBalance :: Amount
  } deriving (Show, Data, Typeable, Generic) -- , ToJSON, FromJSON)

makeLenses ''Account

instance ToJSON Account
instance FromJSON Account

data CommandF n =
    Open AccountName (Account -> n)
  | Close Account n
  | Debit AccountId Double n
  | Credit AccountId Double n
  deriving (Functor)

data Event =
    AccountOpened Account
  | AccountClosed Account
  | AccountDebited AccountId Amount
  | AccountCredited AccountId Amount
  deriving (Show, Data, Generic, Typeable) -- , ToJSON, FromJSON)
-- mkCmd :: (MonadFree CommandF m) => (a -> CommandF a) -> m a
-- mkCmd f = liftF $! f id
instance ToJSON Event
instance FromJSON Event

instance ToJSON AccountId where
  toJSON = toJSON . T.decodeUtf8 . UUID.toASCIIBytes . unaccId
  {-# INLINE toJSON #-}

instance FromJSON AccountId where
  parseJSON = Aeson.withText "AccountId" $ \t ->
                 case UUID.fromASCIIBytes (T.encodeUtf8 t) of
                    Just a  -> return (AccountId a)
                    Nothing -> fail "Bad UUID"
