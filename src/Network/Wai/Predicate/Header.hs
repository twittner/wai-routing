-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Network.Wai.Predicate.Header
    ( Header (..)
    , Hdr    (..)
    , HdrOpt (..)
    , HdrDef (..)
    , HasHdr (..)
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Read
import Data.CaseInsensitive (mk)
import Data.List (find)
import Data.Maybe
import Data.Monoid
import Data.Predicate
import Network.HTTP.Types.Status
import Network.Wai.Predicate.Error
import Network.Wai.Predicate.Internal
import Network.Wai.Predicate.Request

-- | The most generic request header predicate provided.
-- It will get all request header values of 'hdrName' and pass them on to
-- the conversion function 'hdrRead', which might either yield an error
-- message or a value. If the header is not present, an optional default may be
-- returned instead, if nothing is provided, the error message will be used
-- when construction the 400 status.
data Header a = Header
    { hdrName :: !ByteString
    , hdrRead :: [ByteString] -> Either ByteString a
    , hdrDef  :: Maybe a
    }

instance Predicate (Header a) Request where
    type FVal (Header a)     = Error
    type TVal (Header a)     = a
    apply (Header nme f def) =
        rqApply (lookupHeader nme)
                def
                f
                (err status400 ("Missing header '" <> nme <> "'."))

instance Show (Header a) where
    show p = "Header: " ++ show (hdrName p)

-- | Specialisation of 'Header' which returns the first request
-- header value which could be converted to the target type.
-- Relies on 'Readable' type-class for the actual conversion.
newtype Hdr a = Hdr ByteString

instance (Readable a) => Predicate (Hdr a) Request where
    type FVal (Hdr a) = Error
    type TVal (Hdr a) = a
    apply (Hdr x)     = apply (Header x readValues Nothing)

instance Show (Hdr a) where
    show (Hdr n) = "Hdr: " ++ show n

-- | Specialisation of 'Header' which returns the first request
-- header value which could be converted to the target type.
-- If the header is not present, the provided default will be used.
-- Relies on 'Readable' type-class for the actual conversion.
data HdrDef a = HdrDef ByteString a

instance (Readable a) => Predicate (HdrDef a) Request where
    type FVal (HdrDef a) = Error
    type TVal (HdrDef a) = a
    apply (HdrDef x d)   = apply (Header x readValues (Just d))

instance (Show a) => Show (HdrDef a) where
    show (HdrDef x d) = "HdrDef: " ++ show x ++ " [" ++ show d ++ "]"

-- | Predicate which returns the first request header which could be
-- converted to the target type wrapped in a Maybe.
-- If the header is not present, 'Nothing' will be returned.
-- Relies on 'Readable' type-class for the actual conversion.
newtype HdrOpt a = HdrOpt ByteString

instance (Readable a) => Predicate (HdrOpt a) Request where
    type FVal (HdrOpt a) = Error
    type TVal (HdrOpt a) = Maybe a
    apply (HdrOpt x)     = rqApplyMaybe (lookupHeader x) readValues

instance Show (HdrOpt a) where
    show (HdrOpt n) = "HdrOpt: " ++ show n

-- | Predicate which is true if the request has a header with the
-- given name.
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
