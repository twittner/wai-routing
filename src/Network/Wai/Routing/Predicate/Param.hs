-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Wai.Routing.Predicate.Param
    ( Param
    , HasParam
    , param
    , hasParam
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.From
import Network.Wai.Routing.Error
import Network.Wai.Routing.Request
import Network.Wai.Routing.Predicate.Capture
import Network.Wai.Routing.Predicate.Query
import Network.Wai.Routing.Predicate.Predicate

-- | @Param \"x\"@ is equivalent to @'Query' \"x\" ':|:' 'Capture' \"x\"@.
newtype Param a = Param ByteString

param :: ByteString -> Param a
param = Param
{-# INLINABLE param #-}

instance (FromByteString a) => Predicate (Param a) Req where
    type FVal (Param a) = Error
    type TVal (Param a) = a
    apply (Param x)     = apply (query x :|: capture x)

newtype HasParam = HasParam ByteString

hasParam :: ByteString -> HasParam
hasParam = HasParam
{-# INLINABLE hasParam #-}

instance Predicate HasParam Req where
    type FVal HasParam = Error
    type TVal HasParam = ()
    apply (HasParam x) = apply (hasQuery x :|: hasCapture x)

