-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Wai.Routing.Request
    ( Request
    , GetRequest (..)
    , fromWaiRequest
    , waiRequest
    , method
    , headers
    , lookupHeader
    , lookupCapture
    , lookupQuery
    ) where

import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.Maybe (mapMaybe)
import Network.HTTP.Types
import Network.Wai.Routing.Predicate.Predicate

import qualified Network.Wai as Wai

data Request = Request
    { captures :: [(ByteString, ByteString)]
    , request  :: Wai.Request
    }

data GetRequest a = GetRequest

instance Predicate (GetRequest a) Request where
    type FVal (GetRequest a) = a
    type TVal (GetRequest a) = Wai.Request
    apply GetRequest r       = T 0 (request r)

instance Show (GetRequest a) where
    show GetRequest = "Request"

fromWaiRequest :: [(ByteString, ByteString)] -> Wai.Request -> Request
fromWaiRequest = Request

waiRequest :: Request -> Wai.Request
waiRequest = request

headers :: Request -> RequestHeaders
headers = Wai.requestHeaders . request

method :: Request -> Method
method = Wai.requestMethod . request

lookupHeader :: ByteString -> Request -> [ByteString]
lookupHeader name = map snd
                  . filter ((mk name ==) . fst)
                  . Wai.requestHeaders
                  . request

lookupCapture :: ByteString -> Request -> [ByteString]
lookupCapture name = map snd . filter ((name ==) . fst) . captures

lookupQuery :: ByteString -> Request -> [ByteString]
lookupQuery name = mapMaybe snd
                 . filter ((name ==) . fst)
                 . Wai.queryString
                 . request
