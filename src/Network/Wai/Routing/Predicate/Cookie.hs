-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Wai.Routing.Predicate.Cookie
    ( Cookie    (..)
    , HasCookie (..)
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.From
import Data.Monoid
import Network.HTTP.Types.Status
import Network.Wai.Routing.Error
import Network.Wai.Routing.Internal
import Network.Wai.Routing.Predicate.Predicate
import Network.Wai.Routing.Request

newtype Cookie a = Cookie ByteString

instance (FromByteString a) => Predicate (Cookie a) Req where
    type FVal (Cookie a) = Error
    type TVal (Cookie a) = a
    apply (Cookie x)     =
        rqApply (lookupCookie x) readValues (err status400 (msg x))

newtype HasCookie = HasCookie ByteString

instance Predicate HasCookie Req where
    type FVal HasCookie   = Error
    type TVal HasCookie   = ()
    apply (HasCookie x) r =
        if null (lookupCookie x r)
            then F (err status400 (msg x))
            else T 0 ()

msg :: ByteString -> ByteString
msg x = "Missing cookie '" <> x <> "'."
