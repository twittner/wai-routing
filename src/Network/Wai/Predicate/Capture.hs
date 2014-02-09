-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Wai.Predicate.Capture
    ( Capture
    , CaptureOpt
    , CaptureDef
    , HasCapture

    , capture
    , captureOpt
    , captureDef
    , hasCapture
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Readable
import Data.Monoid
import Data.Predicate.Typeof
import Data.Proxy
import Data.Predicate
import Data.Predicate.Descr hiding (tags)
import Network.HTTP.Types.Status
import Network.Wai.Predicate.Error
import Network.Wai.Predicate.Internal
import Network.Wai.Predicate.Request

-- | The most generic request capture predicate provided.
-- It will get all request capture values of '_name' and pass them on to
-- the conversion function '_read', which might either yield an error
-- message or a value. If the capture is not present, an optional default may be
-- returned instead, if nothing is provided, the error message will be used
-- when construction the 400 status.
data RqCapture a = RqCapture
    { _pName    :: !ByteString
    , _pRead    :: [ByteString] -> Either ByteString a
    , _pDefault :: Maybe a
    , _pProxy   :: Proxy a
    }

{-# INLINE rqCapture #-}
rqCapture :: ByteString
          -- ^ request rqCapture name
          -> ([ByteString] -> Either ByteString a)
          -- ^ conversion function
          -> Maybe a
          -- ^ optional default value
          -> RqCapture a
rqCapture n r d = RqCapture n r d Proxy

instance Predicate (RqCapture a) Request where
    type FVal (RqCapture a)       = Error
    type TVal (RqCapture a)       = a
    apply (RqCapture nme f def _) =
        rqApply (lookupCapture nme)
                def
                f
                (err status400 ("Missing path parameter '" <> nme <> "'."))

instance (Typeof a) => Show (RqCapture a) where
    show (RqCapture n _ _ x) =
        "RqCapture: " ++ show n ++ " :: " ++ show (typeof x)

instance (Show a, Typeof a) => Description (RqCapture a) where
    describe (RqCapture n _ d x) =
        DValue (unpack n) (typeof x) (maybe Required (Default . show) d) tags

-- | Specialised capture which returns the first request
-- capture which could be converted to the target type.
-- Relies on 'Readable' type-class for the actual conversion.
data Capture a = Capture ByteString (Proxy a)

{-# INLINE capture #-}
capture :: ByteString -> Capture a
capture n = Capture n Proxy

instance (Readable a) => Predicate (Capture a) Request where
    type FVal (Capture a) = Error
    type TVal (Capture a) = a
    apply (Capture x _)   = apply (rqCapture x readValues Nothing)

instance (Typeof a) => Show (Capture a) where
    show (Capture n x) = "Capture: " ++ show n ++ " :: " ++ show (typeof x)

instance (Typeof a) => Description (Capture a) where
    describe (Capture n x) = DValue (unpack n) (typeof x) Required tags

-- | Specialised capture which returns the first request
-- capture which could be converted to the target type.
-- If the capture is not present, the provided default will be used.
-- Relies on 'Readable' type-class for the actual conversion.
data CaptureDef a = CaptureDef ByteString a

{-# INLINE captureDef #-}
captureDef :: ByteString -> a -> CaptureDef a
captureDef = CaptureDef

instance (Readable a) => Predicate (CaptureDef a) Request where
    type FVal (CaptureDef a) = Error
    type TVal (CaptureDef a) = a
    apply (CaptureDef x d)   = apply (rqCapture x readValues (Just d))

instance (Show a, Typeof a) => Show (CaptureDef a) where
    show (CaptureDef x d) =
        "CaptureDef: " ++ show x ++ " [" ++ show d ++ "] :: " ++ show (typeof d)

instance (Show a, Typeof a) => Description (CaptureDef a) where
    describe (CaptureDef n x) = DValue (unpack n) (typeof x) (Default (show x)) tags

-- | Predicate which returns the first request capture which could be
-- converted to the target type wrapped in a Maybe.
-- If the capture is not present, 'Nothing' will be returned.
-- Relies on 'Readable' type-class for the actual conversion.
data CaptureOpt a = CaptureOpt ByteString (Proxy a)

{-# INLINE captureOpt #-}
captureOpt :: ByteString -> CaptureOpt a
captureOpt n = CaptureOpt n Proxy

instance (Readable a) => Predicate (CaptureOpt a) Request where
    type FVal (CaptureOpt a) = Error
    type TVal (CaptureOpt a) = Maybe a
    apply (CaptureOpt x _)   = rqApplyMaybe (lookupQuery x) readValues

instance (Typeof a) => Show (CaptureOpt a) where
    show (CaptureOpt n x) = "CaptureOpt: " ++ show n ++ " :: " ++ show (typeof x)

instance (Typeof a) => Description (CaptureOpt a) where
    describe (CaptureOpt n x) = DValue (unpack n) (typeof x) Optional tags

-- | Predicate which is true if the request has a capture with the
-- given name.
data HasCapture = HasCapture ByteString

{-# INLINE hasCapture #-}
hasCapture :: ByteString -> HasCapture
hasCapture = HasCapture

instance Predicate HasCapture Request where
    type FVal HasCapture   = Error
    type TVal HasCapture   = ()
    apply (HasCapture x) r =
        if null (lookupQuery x r)
            then F (err status400 ("Missing path parameter '" <> x <> "'."))
            else T 0 ()

instance Show HasCapture where
    show (HasCapture x) = "HasCapture: " ++ show x

instance Description HasCapture where
    describe (HasCapture n) = DValue (unpack n) (TName "()") Required tags

tags :: [Tag]
tags = ["Capture"]

