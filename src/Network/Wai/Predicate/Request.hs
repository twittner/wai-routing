-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Network.Wai.Predicate.Request
    ( Request
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

import qualified Network.Wai as Wai

data Request = Request
    { captures :: [(ByteString, ByteString)]
    , request  :: Wai.Request
    }

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
