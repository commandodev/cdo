{-# LANGUAGE OverloadedStrings #-}
module Cdo.Query where

-- import Control.Monad (join)
import Control.Applicative
import Data.ByteString (ByteString)
-- import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as T
-- import Database.Redis (RedisCtx)
import Database.Redis
import qualified Data.UUID                  as UUID
import Data.Attoparsec.ByteString.Char8
import Cdo.Types



mkAccountNameKey :: AccountName -> ByteString
mkAccountNameKey name = "account-name:" <> (T.encodeUtf8 name)

mkAccountKey :: AccountId -> ByteString
mkAccountKey (AccountId aid)= "account:" <> (UUID.toASCIIBytes aid)

accountExists :: RedisCtx m f => AccountName -> m (f Bool)
accountExists = exists . mkAccountNameKey

accountBalance :: (RedisCtx m f, Functor f) => AccountId -> m (f (Maybe Amount))
accountBalance aid = do
  bal <- hget (mkAccountKey aid) "balance"
  -- traceShow <$> bal
  return $ fmap (>>=  parseAmount) bal

parseAmount :: ByteString -> Maybe Amount
parseAmount bs = Amount <$> (either (const Nothing) Just $ parseOnly double bs)
