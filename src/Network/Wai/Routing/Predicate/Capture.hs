-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Wai.Routing.Predicate.Capture
    ( Capture    (..)
    , HasCapture (..)
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

instance (FromByteString a) => Predicate (Capture a) Req where
    type FVal (Capture a) = Error
    type TVal (Capture a) = a
    apply (Capture x)     =
        let msg = "Missing path parameter '" <> x <> "'." in
        rqApply (lookupCapture x) readValues (err status400 msg)

newtype HasCapture = HasCapture ByteString

instance Predicate HasCapture Req where
    type FVal HasCapture   = Error
    type TVal HasCapture   = ()
    apply (HasCapture x) r =
        if null (lookupQuery x r)
            then F (err status400 ("Missing path parameter '" <> x <> "'."))
            else T 0 ()
