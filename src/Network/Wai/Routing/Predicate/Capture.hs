-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Predicates against path parameters.
-- When declaring routes, paths may contain \"variables\" which
-- capture whatever is given at that position by an actual request.
-- For example:
--
-- @
-- get \"\/user\/:name\/address\/:street\" handler $
--     Capture \"name\" :&: Capture \"street\"
-- @
--
-- extracts from a request path whatever is given for @:name@
-- and @:street@.
module Network.Wai.Routing.Predicate.Capture
    ( Capture
    , HasCapture
    , capture
    , hasCapture
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.From
import Data.Monoid
import Network.HTTP.Types.Status
import Network.Wai.Routing.Error
import Network.Wai.Routing.Internal
import Network.Wai.Routing.Predicate.Predicate
import Network.Wai.Routing.Request

newtype Capture a = Capture ByteString

capture :: ByteString -> Capture a
capture = Capture
{-# INLINABLE capture #-}

instance (FromByteString a) => Predicate (Capture a) Req where
    type FVal (Capture a) = Error
    type TVal (Capture a) = a
    apply (Capture x)     =
        let msg = "Missing path parameter '" <> x <> "'." in
        rqApply (lookupCapture x) readValues (err status400 msg)

newtype HasCapture = HasCapture ByteString

hasCapture :: ByteString -> HasCapture
hasCapture = HasCapture
{-# INLINABLE hasCapture #-}

instance Predicate HasCapture Req where
    type FVal HasCapture   = Error
    type TVal HasCapture   = ()
    apply (HasCapture x) r =
        if null (lookupQuery x r)
            then F (err status400 ("Missing path parameter '" <> x <> "'."))
            else T 0 ()
