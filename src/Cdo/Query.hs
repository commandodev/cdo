{-# LANGUAGE OverloadedStrings #-}
module Cdo.Query where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as T
import Database.Redis (RedisCtx)
import Database.Redis as Redis

import Cdo.Types

mkAccountKey :: AccountName -> ByteString
mkAccountKey accName = "account" <> (T.encodeUtf8 accName)

accountExists :: RedisCtx m f => AccountName -> m (f Bool)
accountExists = exists . mkAccountKey
