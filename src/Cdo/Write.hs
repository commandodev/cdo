{-# LANGUAGE OverloadedStrings #-}
module Cdo.Write where


import qualified Data.ByteString.Lazy   as LBS
import           Database.Redis

--import qualified Pipes.Prelude as P
import           Data.Aeson             (ToJSON)
import qualified Data.Aeson             as Aeson
import Cdo.Types

writeEvent :: (RedisCtx m f, ToJSON e) => e -> m (f Integer)
writeEvent evt = lpush "events" [LBS.toStrict $ Aeson.encode evt]

saveAmount :: (RedisCtx m f) => Account -> m (f Status)
saveAmount = undefined

createAccount :: RedisCtx m f => Account -> m (f Status)
createAccount = undefined

deleteAccount :: RedisCtx m f => AccountId -> m (f Integer)
deleteAccount = undefined
