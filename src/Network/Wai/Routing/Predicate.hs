-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Routing.Predicate where

import Data.ByteString (ByteString)
import Data.ByteString.From
import Data.Monoid
import Network.HTTP.Types
import Network.Wai.Predicate
import Network.Wai.Predicate.Request
import Network.Wai.Predicate.Utility
import Network.Wai.Routing.Request

capture :: (HasCaptures r, FromByteString a) => ByteString -> Predicate r Error a
capture k r = case lookupCapture k r of
    [] -> Fail (err status400 ("Missing path parameter '" <> k <> "'."))
    cc -> either (Fail . err status400) return (readValues cc)

hasCapture :: (HasCaptures r) => ByteString -> Predicate r Error ()
hasCapture k r =
    if null (lookupCapture k r)
        then Fail (err status400 ("Missing path parameter '" <> k <> "'."))
        else return ()

param :: (HasCaptures r, HasQuery r, FromByteString a) => ByteString -> Predicate r Error a
param k = query k .|. capture k

hasParam :: (HasCaptures r, HasQuery r) => ByteString -> Predicate r Error ()
hasParam k = hasQuery k .|. hasCapture k
