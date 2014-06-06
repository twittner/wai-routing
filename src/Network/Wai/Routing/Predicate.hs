-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

-- | 'Predicate's which are specific to @wai-routing@.
-- Please note that these can be freely combined with other predicates from
-- @wai-predicates@.
module Network.Wai.Routing.Predicate where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.From
import Network.Wai.Predicate
import Network.Wai.Predicate.Request
import Network.Wai.Predicate.Utility
import Network.Wai.Routing.Request

-- | Request path parameters.
capture :: (HasCaptures r, FromByteString a) => ByteString -> Predicate r Error a
capture k r = case lookupCapture k r of
    [] -> Fail $ e400 & addLabel "path" . setReason NotAvailable . setSource k
    cc -> either (\m -> Fail $ e400 & addLabel "path" . setReason TypeError . setSource k . setMessage m)
                 return
                 (readValues cc)

-- | Request path parameters.
hasCapture :: (HasCaptures r) => ByteString -> Predicate r Error ()
hasCapture k r =
    when (null (lookupCapture k r)) $
        Fail (e400 & addLabel "path" . setReason NotAvailable . setSource k)

-- | @param \"foo\"@ is equivalent to @query \"foo\" .|. capture \"foo\"@
param :: (HasCaptures r, HasQuery r, FromByteString a) => ByteString -> Predicate r Error a
param k = query k .|. capture k

hasParam :: (HasCaptures r, HasQuery r) => ByteString -> Predicate r Error ()
hasParam k = hasQuery k .|. hasCapture k
