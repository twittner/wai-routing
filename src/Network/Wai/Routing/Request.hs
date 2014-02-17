-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A wrapped WAI 'Request' which holds additional data of interest
-- only to 'Predicate' authors.
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
    , lookupCookie
    ) where

import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.Maybe (mapMaybe)
import Network.HTTP.Types
import Network.Wai (Request)
import Network.Wai.Routing.Predicate.Predicate
import Web.Cookie

import qualified Network.Wai as Wai

data Req = Req
    { captures :: [(ByteString, ByteString)]
    , request  :: Request
    , cookies  :: Cookies
    }

-- | A 'Predicate' which just returns the WAI 'Wai.Request'.
-- By including this predicate, handlers have easy access to
-- the complete request.
data GetRequest a = GetRequest

instance Predicate (GetRequest a) Req where
    type FVal (GetRequest a) = a
    type TVal (GetRequest a) = Request
    apply GetRequest r       = T 0 (request r)

fromWaiRequest :: [(ByteString, ByteString)] -> Request -> Req
fromWaiRequest ca rq =
    Req ca rq (concatMap parseCookies (getHeaders "Cookie" rq))

waiRequest :: Req -> Request
waiRequest = request

headers :: Req -> RequestHeaders
headers = Wai.requestHeaders . request

method :: Req -> Method
method = Wai.requestMethod . request

lookupHeader :: ByteString -> Req -> [ByteString]
lookupHeader name = getHeaders name . request

lookupCapture :: ByteString -> Req -> [ByteString]
lookupCapture name = map snd . filter ((name ==) . fst) . captures

lookupCookie :: ByteString -> Req -> [ByteString]
lookupCookie name = map snd . filter ((name ==) . fst) . cookies

lookupQuery :: ByteString -> Req -> [ByteString]
lookupQuery name = mapMaybe snd
                 . filter ((name ==) . fst)
                 . Wai.queryString
                 . request

getHeaders :: ByteString -> Wai.Request -> [ByteString]
getHeaders name = map snd . filter ((mk name ==) . fst) . Wai.requestHeaders

