{-# LANGUAGE OverloadedStrings #-}
module Cdo.Write where


import qualified Data.ByteString.Lazy   as LBS
import           Database.Redis

--import qualified Pipes.Prelude as P
import Data.String (fromString)
import           Data.Aeson             (ToJSON)
import qualified Data.Aeson             as Aeson
import qualified Data.Text.Encoding as T
import Cdo.Types
import Cdo.Query

writeEvent :: (RedisCtx m f, ToJSON e) => e -> m (f Integer)
writeEvent evt = lpush "events" [LBS.toStrict $ Aeson.encode evt]

saveAmount :: (RedisCtx m f) => AccountId -> Amount -> m (f Bool)
saveAmount aid (Amount amt) = hset (mkAccountKey aid) "balance" (fromString $ show amt)

writeAccount :: RedisCtx m f => Account -> m (f Status)
writeAccount (Account aid name (Amount amt)) = hmset (mkAccountKey aid) kvs
  where
    kvs = [
            ("name", T.encodeUtf8 name)
          , ("balance", fromString (show amt))
          ]

deleteAccount :: RedisCtx m f => AccountId -> m (f Integer)
deleteAccount = undefined
