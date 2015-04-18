{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Cdo.Query where

import Control.Monad (join)
import Control.Applicative
-- import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
-- import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as T
-- import Database.Redis (RedisCtx)
import Control.Error.Util (note)
import Database.Redis
import           Control.Monad.Trans.Either (EitherT (..))
import qualified Data.UUID                  as UUID
import Data.Attoparsec.ByteString.Char8
import Cdo.Types



mkAccountNameKey :: AccountName -> ByteString
mkAccountNameKey name = "account-name:" <> T.encodeUtf8 name

mkAccountKey :: AccountId -> ByteString
mkAccountKey (AccountId aid) = "account:" <> UUID.toASCIIBytes aid

accountExists :: RedisCtx m f => AccountName -> m (f Bool)
accountExists = exists . mkAccountNameKey

accountBalance :: (RedisCtx m (Either Reply)) => AccountId -> m (Either Reply Amount)
accountBalance aid = do
  Right bal <- hget (mkAccountKey aid) "balance"
  return $ note (Error "Couldn't parse amount") $ join $ (parseAmount <$> bal)

getAccount :: (RedisCtx m (Either Reply)) => AccountName -> m (Either Reply Account)
getAccount name = runEitherT $ do
  uuid' <- EitherT $ get (mkAccountNameKey name)
  uuid <- AccountId <$> (EitherT . return . err "Couldn't parse UUID" $ UUID.fromASCIIBytes =<< uuid')
  bal <- EitherT $ accountBalance uuid
  return $ Account uuid name bal
  where err msg = note (Error msg)

parseAmount :: ByteString -> Maybe Amount
parseAmount bs = Amount <$> either (const Nothing) Just (parseOnly double bs)
