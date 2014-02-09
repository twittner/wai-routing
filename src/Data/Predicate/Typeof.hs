-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Predicate.Typeof where

import Data.Monoid
import Data.Predicate.Descr
import Data.Proxy
import GHC.Generics

import Data.Int
import Data.Word
import Data.Text (Text)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L

class Typeof a where
    typeof :: a -> Type

    default typeof :: (Generic a, GTypeof (Rep a)) => a -> Type
    typeof a = gtypeof (undefined `asTypeOf` (from a))

class GTypeof f where
    gtypeof :: f a -> Type

instance GTypeof U1 where
    gtypeof _ = TName "()"

instance (Typeof a) => GTypeof (K1 R a) where
    gtypeof _ = typeof (undefined :: a)

instance (Datatype c, GTypeof f) => GTypeof (D1 c f) where
    gtypeof (m1 :: (D1 c f) a) =
        Type (datatypeName m1) (gtypeof (undefined :: f a))

instance (Constructor c, GTypeof f) => GTypeof (C1 c f) where
    gtypeof (m1 :: (C1 c f) a) = TDCon (conName m1) (gtypeof (undefined :: f a))

instance (GTypeof f, GTypeof g) => GTypeof (f :*: g) where
    gtypeof (_ :: (f :*: g) a) =
        let p1 = gtypeof (undefined :: f a)
            p2 = gtypeof (undefined :: g a)
        in case (p1, p2) of
            (TRecd x, TRecd y) -> TRecd  (x <> y)
            (TProd x, TProd y) -> TProd (x <> y)
            _                  -> TProd [p1, p2]

instance (Selector s, GTypeof f) => GTypeof (S1 s f) where
    gtypeof (m1 :: (S1 s f) a) = case selName m1 of
        "" -> TProd [gtypeof (undefined :: f a)]
        nm -> TRecd [(nm, gtypeof (undefined :: f a))]

instance (GTypeof f, GTypeof g) => GTypeof (f :+: g) where
    gtypeof (_ :: (f :+: g) a) =
        let s1 = gtypeof (undefined :: f a)
            s2 = gtypeof (undefined :: g a)
        in case (s1, s2) of
            (TSum x, TSum y) -> TSum (x <> y)
            _                -> TSum [s1, s2]

instance Typeof Int where typeof _ = TName "Int"
instance Typeof Int8 where typeof _ = TName "Int8"
instance Typeof Int16 where typeof _ = TName "Int16"
instance Typeof Int32 where typeof _ = TName "Int32"
instance Typeof Int64 where typeof _ = TName "Int64"
instance Typeof Integer where typeof _ = TName "Integer"
instance Typeof Bool where typeof _ = TName "Bool"
instance Typeof Char where typeof _ = TName "Char"
instance Typeof Float where typeof _ = TName "Float"
instance Typeof Double where typeof _ = TName "Double"
instance Typeof () where typeof _ = TName "()"
instance Typeof Text where typeof _ = TName "Text"
instance Typeof B.ByteString where typeof _ = TName "ByteString"
instance Typeof L.ByteString where typeof _ = TName "ByteString"
instance Typeof Word where typeof _ = TName "Word"
instance Typeof Word8 where typeof _ = TName "Word8"
instance Typeof Word16 where typeof _ = TName "Word16"
instance Typeof Word32 where typeof _ = TName "Word32"
instance Typeof Word64 where typeof _ = TName "Word64"
instance (Typeof a) => Typeof (Maybe a)
instance (Typeof a, Typeof b) => Typeof (Either a b)

instance (Typeof a, Typeof b) => Typeof (a, b)
instance (Typeof a, Typeof b, Typeof c) => Typeof (a, b, c)
instance (Typeof a, Typeof b, Typeof c, Typeof d) => Typeof (a, b, c, d)
instance (Typeof a, Typeof b, Typeof c, Typeof d, Typeof e) => Typeof (a, b, c, d, e)

instance (Typeof a) => Typeof (Proxy a) where
    typeof _ = typeof (undefined :: a)

instance (Typeof a) => Typeof [a] where
    typeof _ = Type "List" (typeof (undefined :: a))
