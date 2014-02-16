-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Wai.Routing.Request
    ( Req
    , GetRequest (..)
    , fromWaiRequest
    , waiRequest
    , method
    , headers
    , lookupHeader
    , lookupCapture
    , lookupQuery
    ) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.Maybe (mapMaybe)
import Network.HTTP.Types
import Network.Wai (Request)
import Network.Wai.Routing.Predicate.Predicate

import qualified Network.Wai as Wai

data Req = Req
    { captures :: [(ByteString, ByteString)]
    , request  :: Request
    }

data GetRequest a = GetRequest

instance Applicative m => Predicate m (GetRequest a) Req where
    type FVal (GetRequest a) = a
    type TVal (GetRequest a) = Request
    apply GetRequest r       = pure $ T 0 (request r)

fromWaiRequest :: [(ByteString, ByteString)] -> Request -> Req
fromWaiRequest = Req

waiRequest :: Req -> Request
waiRequest = request

headers :: Req -> RequestHeaders
headers = Wai.requestHeaders . request

method :: Req -> Method
method = Wai.requestMethod . request

lookupHeader :: ByteString -> Req -> [ByteString]
lookupHeader name = map snd
                  . filter ((mk name ==) . fst)
                  . Wai.requestHeaders
                  . request

lookupCapture :: ByteString -> Req -> [ByteString]
lookupCapture name = map snd . filter ((name ==) . fst) . captures

lookupQuery :: ByteString -> Req -> [ByteString]
lookupQuery name = mapMaybe snd
                 . filter ((name ==) . fst)
                 . Wai.queryString
                 . request
