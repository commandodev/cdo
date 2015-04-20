{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Cdo.Query where

import Control.Lens
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
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS


hGetHashMap :: (RedisCtx m f, Functor m, Functor f) => AccountId -> m (f (HashMap ByteString ByteString))
hGetHashMap (mkAccountKey -> key) = fmap HMS.fromList <$> hgetall key

getAccountById :: (RedisCtx m (Either Reply), Functor m) => AccountId -> m (Either Reply Account)
getAccountById aid = do
  Right hm <- hGetHashMap aid
  return $ Account aid <$> (T.decodeUtf8 <$> hm .: "name")
                       <*> (hm .: "balance" >>= (err "Couldn't parse balance" . parseAmount))
  where
    hm .: k = (err $ "Couldn't find key " <> k) $ (hm ^. at k)

mkAccountNameKey :: AccountName -> ByteString
mkAccountNameKey name = "account-name:" <> T.encodeUtf8 name

mkAccountKey :: AccountId -> ByteString
mkAccountKey (AccountId aid) = "account:" <> UUID.toASCIIBytes aid

accountExists :: RedisCtx m f => AccountName -> m (f Bool)
accountExists = exists . mkAccountNameKey

accountBalance :: (RedisCtx m (Either Reply)) => AccountId -> m (Either Reply Amount)
accountBalance aid = do
  Right bal <- hget (mkAccountKey aid) "balance"
  return $ note (Error "Couldn't parse amount") $ join (parseAmount <$> bal)

getAccount :: (RedisCtx m (Either Reply)) => AccountName -> m (Either Reply Account)
getAccount name = runEitherT $ do
  uuid' <- EitherT $ get (mkAccountNameKey name)
  uuid <- AccountId <$> (EitherT . return . err "Couldn't parse UUID" $ UUID.fromASCIIBytes =<< uuid')
  bal <- EitherT $ accountBalance uuid
  return $ Account uuid name bal
  where 

parseAmount :: ByteString -> Maybe Amount
parseAmount bs = Amount <$> either (const Nothing) Just (parseOnly double bs)


err :: ByteString -> Maybe b -> Either Reply b
err msg = note (Error msg)
