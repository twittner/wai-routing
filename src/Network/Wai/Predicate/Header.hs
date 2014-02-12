-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Network.Wai.Predicate.Header
    ( Hdr    (..)
    , HasHdr (..)
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.From
import Data.CaseInsensitive (mk)
import Data.List (find)
import Data.Maybe
import Data.Monoid
import Data.Predicate
import Network.HTTP.Types.Status
import Network.Wai.Predicate.Error
import Network.Wai.Predicate.Internal
import Network.Wai.Predicate.Request

newtype Hdr a = Hdr ByteString

instance (FromByteString a) => Predicate (Hdr a) Request where
    type FVal (Hdr a) = Error
    type TVal (Hdr a) = a
    apply (Hdr x)     =
        let msg = "Missing header '" <> x <> "'." in
        rqApply (lookupHeader x) readValues (err status400 msg)

instance Show (Hdr a) where
    show (Hdr n) = "Hdr: " ++ show n

newtype HasHdr = HasHdr ByteString

instance Predicate HasHdr Request where
    type FVal HasHdr   = Error
    type TVal HasHdr   = ()
    apply (HasHdr x) r =
        if isJust $ find ((mk x ==) . fst) (headers r)
            then T 0 ()
            else F (err status400 ("Missing header '" <> x <> "'."))

instance Show HasHdr where
    show (HasHdr x) = "HasHdr: " ++ show x

