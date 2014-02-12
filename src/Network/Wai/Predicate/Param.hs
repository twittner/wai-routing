-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Wai.Predicate.Param
    ( Param     (..)
    , HasParam  (..)
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Read
import Data.Predicate
import Network.Wai.Predicate.Error
import Network.Wai.Predicate.Capture
import Network.Wai.Predicate.Query
import Network.Wai.Predicate.Request

newtype Param a = Param ByteString

instance (Readable a) => Predicate (Param a) Request where
    type FVal (Param a) = Error
    type TVal (Param a) = a
    apply (Param x)     = apply (Query x :|: Capture x)

instance Show (Param a) where
    show (Param n) = "Param: " ++ show n

newtype HasParam = HasParam ByteString

instance Predicate HasParam Request where
    type FVal HasParam = Error
    type TVal HasParam = ()
    apply (HasParam x) = apply (HasQuery x :|: HasCapture x)

instance Show HasParam where
    show (HasParam x) = "HasParam: " ++ show x

