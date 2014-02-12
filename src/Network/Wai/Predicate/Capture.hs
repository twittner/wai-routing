-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Wai.Predicate.Capture
    ( Capture    (..)
    , HasCapture (..)
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Read
import Data.Monoid
import Data.Predicate
import Network.HTTP.Types.Status
import Network.Wai.Predicate.Error
import Network.Wai.Predicate.Internal
import Network.Wai.Predicate.Request

newtype Capture a = Capture ByteString

instance (Readable a) => Predicate (Capture a) Request where
    type FVal (Capture a) = Error
    type TVal (Capture a) = a
    apply (Capture x)     =
        let msg = "Missing path parameter '" <> x <> "'." in
        rqApply (lookupCapture x) readValues (err status400 msg)

instance Show (Capture a) where
    show (Capture n) = "Capture: " ++ show n

newtype HasCapture = HasCapture ByteString

instance Predicate HasCapture Request where
    type FVal HasCapture   = Error
    type TVal HasCapture   = ()
    apply (HasCapture x) r =
        if null (lookupQuery x r)
            then F (err status400 ("Missing path parameter '" <> x <> "'."))
            else T 0 ()

instance Show HasCapture where
    show (HasCapture x) = "HasCapture: " ++ show x

