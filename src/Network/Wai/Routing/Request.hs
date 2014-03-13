-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Network.Wai.Routing.Request
    ( RoutingReq
    , HasCaptures (..)
    , fromReq
    , lookupCapture
    ) where

import Data.ByteString (ByteString)
import Network.Wai.Predicate.Request

class HasCaptures a where
     captures :: a -> [(ByteString, ByteString)]

data RoutingReq = RoutingReq
    { _captures :: [(ByteString, ByteString)]
    , _request  :: Req
    }

instance HasRequest RoutingReq where
    getRequest = getRequest . _request

instance HasMethod RoutingReq where
    method = method . _request

instance HasHeaders RoutingReq where
    headers = headers . _request

instance HasCookies RoutingReq where
    cookies = cookies . _request

instance HasQuery RoutingReq where
    queryItems = queryItems . _request

instance HasPath RoutingReq where
    segments = segments . _request

instance HasCaptures RoutingReq where
    captures = _captures

instance HasVault RoutingReq where
    requestVault = requestVault . _request

fromReq :: [(ByteString, ByteString)] -> Req -> RoutingReq
fromReq = RoutingReq

lookupCapture :: (HasCaptures r) => ByteString -> r -> [ByteString]
lookupCapture name = map snd . filter ((name ==) . fst) . captures

