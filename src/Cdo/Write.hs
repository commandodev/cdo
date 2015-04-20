{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Cdo.Write where

import           Cdo.Query
import           Cdo.Types
import           Control.Lens         hiding (set)
import           Data.Aeson           (ToJSON)
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.String          (fromString)
import qualified Data.Text.Encoding   as T
import qualified Data.UUID            as UUID
import           Database.Redis

writeEvent :: (RedisCtx m f, ToJSON e) => e -> m (f Integer)
writeEvent evt = lpush "events" [LBS.toStrict $ Aeson.encode evt]

changeAmount :: (RedisCtx m (Either Reply)) => AccountId -> Amount -> m (Either Reply Bool)
changeAmount aid amt = do
  Right bal <- accountBalance aid
  hset k "balance" (fromString $ show (unAmount $ bal + amt))
  where k = mkAccountKey aid

writeAccount :: RedisCtx m f => Account -> m (f Status)
writeAccount (Account aid name (Amount amt)) = do
  _ <- hmset (mkAccountKey aid) kvs
  set (mkAccountNameKey name) $ UUID.toASCIIBytes $ unaccId aid
  where
    kvs = [
            ("name", T.encodeUtf8 name)
          , ("balance", fromString (show amt))
          ]

deleteAccount :: (RedisCtx m (Either Reply), Functor m) => AccountId -> m (Either Reply Integer)
deleteAccount aid = do
  Right acc <- getAccountById aid
  del [mkAccountKey aid, mkAccountNameKey (acc ^. accName)]
