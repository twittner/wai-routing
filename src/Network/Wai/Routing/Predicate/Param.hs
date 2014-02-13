-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Wai.Routing.Predicate.Param
    ( Param     (..)
    , HasParam  (..)
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.From
import Network.Wai.Routing.Error
import Network.Wai.Routing.Request
import Network.Wai.Routing.Predicate.Capture
import Network.Wai.Routing.Predicate.Query
import Network.Wai.Routing.Predicate.Predicate

newtype Param a = Param ByteString

instance (FromByteString a) => Predicate (Param a) Req where
    type FVal (Param a) = Error
    type TVal (Param a) = a
    apply (Param x)     = apply (Query x :|: Capture x)

newtype HasParam = HasParam ByteString

instance Predicate HasParam Req where
    type FVal HasParam = Error
    type TVal HasParam = ()
    apply (HasParam x) = apply (HasQuery x :|: HasCapture x)

