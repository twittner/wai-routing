-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Wai.Predicate.Capture
    ( RqCapture  (..)
    , Capture    (..)
    , CaptureOpt (..)
    , CaptureDef (..)
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

-- | The most generic request capture predicate provided.
-- It will get all request capture values of '_name' and pass them on to
-- the conversion function '_read', which might either yield an error
-- message or a value. If the capture is not present, an optional default may be
-- returned instead, if nothing is provided, the error message will be used
-- when construction the 400 status.
data RqCapture a = RqCapture
    { capName :: !ByteString
    , capRead :: [ByteString] -> Either ByteString a
    , capDef  :: Maybe a
    }

instance Predicate (RqCapture a) Request where
    type FVal (RqCapture a)     = Error
    type TVal (RqCapture a)     = a
    apply (RqCapture nme f def) =
        rqApply (lookupCapture nme)
                def
                f
                (err status400 ("Missing path parameter '" <> nme <> "'."))

instance Show (RqCapture a) where
    show a = "RqCapture: " ++ show (capName a)

-- | Specialised capture which returns the first request
-- capture which could be converted to the target type.
-- Relies on 'Readable' type-class for the actual conversion.
newtype Capture a = Capture ByteString

instance (Readable a) => Predicate (Capture a) Request where
    type FVal (Capture a) = Error
    type TVal (Capture a) = a
    apply (Capture x)     = apply (RqCapture x readValues Nothing)

instance Show (Capture a) where
    show (Capture n) = "Capture: " ++ show n

-- | Specialised capture which returns the first request
-- capture which could be converted to the target type.
-- If the capture is not present, the provided default will be used.
-- Relies on 'Readable' type-class for the actual conversion.
data CaptureDef a = CaptureDef ByteString a

instance (Readable a) => Predicate (CaptureDef a) Request where
    type FVal (CaptureDef a) = Error
    type TVal (CaptureDef a) = a
    apply (CaptureDef x d)   = apply (RqCapture x readValues (Just d))

instance (Show a) => Show (CaptureDef a) where
    show (CaptureDef x d) = "CaptureDef: " ++ show x ++ " [" ++ show d ++ "]"

-- | Predicate which returns the first request capture which could be
-- converted to the target type wrapped in a Maybe.
-- If the capture is not present, 'Nothing' will be returned.
-- Relies on 'Readable' type-class for the actual conversion.
newtype CaptureOpt a = CaptureOpt ByteString

instance (Readable a) => Predicate (CaptureOpt a) Request where
    type FVal (CaptureOpt a) = Error
    type TVal (CaptureOpt a) = Maybe a
    apply (CaptureOpt x)     = rqApplyMaybe (lookupQuery x) readValues

instance Show (CaptureOpt a) where
    show (CaptureOpt n) = "CaptureOpt: " ++ show n

-- | Predicate which is true if the request has a capture with the
-- given name.
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
