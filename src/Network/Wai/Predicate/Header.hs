-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Network.Wai.Predicate.Header
    ( Hdr
    , HdrOpt
    , HdrDef
    , HasHdr

    , hdr
    , hdrOpt
    , hdrDef
    , hasHdr
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Readable
import Data.CaseInsensitive (mk)
import Data.List (find)
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.Predicate
import Data.Predicate.Descr hiding (tags)
import Data.Predicate.Typeof
import Network.HTTP.Types.Status
import Network.Wai.Predicate.Error
import Network.Wai.Predicate.Internal
import Network.Wai.Predicate.Request

-- | The most generic request header predicate provided.
-- It will get all request header values of '_name' and pass them on to
-- the conversion function '_read', which might either yield an error
-- message or a value. If the header is not present, an optional default may be
-- returned instead, if nothing is provided, the error message will be used
-- when construction the 400 status.
data Header a = Header
    { _hdrName    :: !ByteString
    , _hdrRead    :: [ByteString] -> Either ByteString a
    , _hdrDefault :: Maybe a
    , _hdrProxy   :: Proxy a
    }

{-# INLINE header #-}
header :: ByteString
       -- ^ request header name
       -> ([ByteString] -> Either ByteString a)
       -- ^ conversion function
       -> Maybe a
       -- ^ optional default value
       -> Header a
header n r d = Header n r d Proxy

instance Predicate (Header a) Request where
    type FVal (Header a)       = Error
    type TVal (Header a)       = a
    apply (Header nme f def _) =
        rqApply (lookupHeader nme)
                def
                f
                (err status400 ("Missing header '" <> nme <> "'."))

instance Show (Header a) where
    show p = "Header: " ++ show (_hdrName p)

instance (Show a, Typeof a) => Description (Header a) where
    describe (Header n _ d x) =
        DValue (unpack n) (typeof x) (maybe Required (Default . show) d) tags

-- | Specialisation of 'Header' which returns the first request
-- header value which could be converted to the target type.
-- Relies on 'Readable' type-class for the actual conversion.
data Hdr a = Hdr ByteString (Proxy a)

{-# INLINE hdr #-}
hdr :: ByteString -> Hdr a
hdr n = Hdr n Proxy

instance (Readable a) => Predicate (Hdr a) Request where
    type FVal (Hdr a) = Error
    type TVal (Hdr a) = a
    apply (Hdr x _)   = apply (header x readValues Nothing)

instance (Typeof a) => Show (Hdr a) where
    show (Hdr n x) = "Hdr: " ++ show n ++ " :: " ++ show (typeof x)

instance (Typeof a) => Description (Hdr a) where
    describe (Hdr n x) = DValue (unpack n) (typeof x) Required tags

-- | Specialisation of 'Header' which returns the first request
-- header value which could be converted to the target type.
-- If the header is not present, the provided default will be used.
-- Relies on 'Readable' type-class for the actual conversion.
data HdrDef a = HdrDef ByteString a

{-# INLINE hdrDef #-}
hdrDef :: ByteString -> a -> HdrDef a
hdrDef = HdrDef

instance (Readable a) => Predicate (HdrDef a) Request where
    type FVal (HdrDef a) = Error
    type TVal (HdrDef a) = a
    apply (HdrDef x d)   = apply (header x readValues (Just d))

instance (Show a, Typeof a) => Show (HdrDef a) where
    show (HdrDef x d) =
        "HdrDef: " ++ show x ++ " [" ++ show d ++ "] :: " ++ show (typeof d)

instance (Show a, Typeof a) => Description (HdrDef a) where
    describe (HdrDef n x) = DValue (unpack n) (typeof x) (Default (show x)) tags

-- | Predicate which returns the first request header which could be
-- converted to the target type wrapped in a Maybe.
-- If the header is not present, 'Nothing' will be returned.
-- Relies on 'Readable' type-class for the actual conversion.
data HdrOpt a = HdrOpt ByteString (Proxy a)

{-# INLINE hdrOpt #-}
hdrOpt :: ByteString -> HdrOpt a
hdrOpt n = HdrOpt n Proxy

instance (Readable a) => Predicate (HdrOpt a) Request where
    type FVal (HdrOpt a) = Error
    type TVal (HdrOpt a) = Maybe a
    apply (HdrOpt x _)   = rqApplyMaybe (lookupHeader x) readValues

instance (Typeof a) => Show (HdrOpt a) where
    show (HdrOpt n x) = "HdrOpt: " ++ show n ++ " :: " ++ show (typeof x)

instance (Typeof a) => Description (HdrOpt a) where
    describe (HdrOpt n x) = DValue (unpack n) (typeof x) Optional tags

-- | Predicate which is true if the request has a header with the
-- given name.
data HasHdr = HasHdr ByteString

{-# INLINE hasHdr #-}
hasHdr :: ByteString -> HasHdr
hasHdr = HasHdr

instance Predicate HasHdr Request where
    type FVal HasHdr   = Error
    type TVal HasHdr   = ()
    apply (HasHdr x) r =
        if isJust $ find ((mk x ==) . fst) (headers r)
            then T 0 ()
            else F (err status400 ("Missing header '" <> x <> "'."))

instance Show HasHdr where
    show (HasHdr x) = "HasHdr: " ++ show x

instance Description HasHdr where
    describe (HasHdr n) = DValue (unpack n) (TName "()") Required tags

tags :: [Tag]
tags = ["Header"]
