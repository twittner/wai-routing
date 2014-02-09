-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Wai.Predicate.Param
    ( Param
    , ParamOpt
    , ParamDef
    , HasParam

    , param
    , paramOpt
    , paramDef
    , hasParam
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Readable
import Data.Predicate.Typeof
import Data.Proxy
import Data.Predicate
import Data.Predicate.Descr hiding (tags)
import Network.Wai.Predicate.Error
import Network.Wai.Predicate.Capture
import Network.Wai.Predicate.Query
import Network.Wai.Predicate.Request

data Param a = Param ByteString (Proxy a)

{-# INLINE param #-}
param :: ByteString -> Param a
param n = Param n Proxy

instance (Readable a) => Predicate (Param a) Request where
    type FVal (Param a) = Error
    type TVal (Param a) = a
    apply (Param x _)   = apply (query x :|: capture x)

instance (Typeof a) => Show (Param a) where
    show (Param n x) = "Param: " ++ show n ++ " :: " ++ show (typeof x)

instance (Typeof a) => Description (Param a) where
    describe (Param n x) = DValue (unpack n) (typeof x) Required tags


data ParamDef a = ParamDef ByteString a

{-# INLINE paramDef #-}
paramDef :: ByteString -> a -> ParamDef a
paramDef = ParamDef

instance (Readable a) => Predicate (ParamDef a) Request where
    type FVal (ParamDef a) = Error
    type TVal (ParamDef a) = a
    apply (ParamDef x d)   = apply (queryDef x d :|: captureDef x d)

instance (Show a, Typeof a) => Show (ParamDef a) where
    show (ParamDef x d) =
        "ParamDef: " ++ show x ++ " [" ++ show d ++ "] :: " ++ show (typeof d)

instance (Show a, Typeof a) => Description (ParamDef a) where
    describe (ParamDef n x) = DValue (unpack n) (typeof x) (Default (show x)) tags


data ParamOpt a = ParamOpt ByteString (Proxy a)

{-# INLINE paramOpt #-}
paramOpt :: ByteString -> ParamOpt a
paramOpt n = ParamOpt n Proxy

instance (Readable a) => Predicate (ParamOpt a) Request where
    type FVal (ParamOpt a) = Error
    type TVal (ParamOpt a) = Maybe a
    apply (ParamOpt x _)   = apply (queryOpt x :|: captureOpt x)

instance (Typeof a) => Show (ParamOpt a) where
    show (ParamOpt n x) = "ParamOpt: " ++ show n ++ " :: " ++ show (typeof x)

instance (Typeof a) => Description (ParamOpt a) where
    describe (ParamOpt n x) = DValue (unpack n) (typeof x) Optional tags


data HasParam = HasParam ByteString

{-# INLINE hasParam #-}
hasParam :: ByteString -> HasParam
hasParam = HasParam

instance Predicate HasParam Request where
    type FVal HasParam = Error
    type TVal HasParam = ()
    apply (HasParam x) = apply (hasQuery x :|: hasCapture x)

instance Show HasParam where
    show (HasParam x) = "HasParam: " ++ show x

instance Description HasParam where
    describe (HasParam n) = DValue (unpack n) (TName "()") Required tags


tags :: [Tag]
tags = ["Param"]

