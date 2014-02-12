-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Wai.Predicate.Param
    ( Parameter (..)
    , Param     (..)
    , ParamOpt  (..)
    , ParamDef  (..)
    , HasParam  (..)
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Read
import Data.Predicate
import Network.Wai.Predicate.Error
import Network.Wai.Predicate.Capture
import Network.Wai.Predicate.Query
import Network.Wai.Predicate.Request

data Parameter a = Parameter
    { pName :: !ByteString
    , pRead :: [ByteString] -> Either ByteString a
    , pDef  :: Maybe a
    }

instance (Readable a) => Predicate (Parameter a) Request where
    type FVal (Parameter a) = Error
    type TVal (Parameter a) = a
    apply (Parameter n f d) = apply (RqQuery n f d :|: RqCapture n f d)

instance Show (Parameter a) where
    show a = "Parameter: " ++ show (pName a)

newtype Param a = Param ByteString

instance (Readable a) => Predicate (Param a) Request where
    type FVal (Param a) = Error
    type TVal (Param a) = a
    apply (Param x)     = apply (Query x :|: Capture x)

instance Show (Param a) where
    show (Param n) = "Param: " ++ show n

data ParamDef a = ParamDef ByteString a

instance (Readable a) => Predicate (ParamDef a) Request where
    type FVal (ParamDef a) = Error
    type TVal (ParamDef a) = a
    apply (ParamDef x d)   = apply (QueryDef x d :|: CaptureDef x d)

instance (Show a) => Show (ParamDef a) where
    show (ParamDef x d) = "ParamDef: " ++ show x ++ " [" ++ show d ++ "]"

newtype ParamOpt a = ParamOpt ByteString

instance (Readable a) => Predicate (ParamOpt a) Request where
    type FVal (ParamOpt a) = Error
    type TVal (ParamOpt a) = Maybe a
    apply (ParamOpt x)     = apply (QueryOpt x :|: CaptureOpt x)

instance Show (ParamOpt a) where
    show (ParamOpt n) = "ParamOpt: " ++ show n

newtype HasParam = HasParam ByteString

instance Predicate HasParam Request where
    type FVal HasParam = Error
    type TVal HasParam = ()
    apply (HasParam x) = apply (HasQuery x :|: HasCapture x)

instance Show HasParam where
    show (HasParam x) = "HasParam: " ++ show x
