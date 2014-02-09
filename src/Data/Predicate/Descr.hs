-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE BangPatterns #-}

module Data.Predicate.Descr where

type Name = String
type Tag  = String

data Type
    = Type  Name Type
    | TDCon Name Type
    | TProd [Type]
    | TRecd [(Name, Type)]
    | TSum  [Type]
    | TName Name
    deriving (Eq, Show)

data Obligation
    = Required
    | Optional
    | Default !String
    deriving (Eq, Show)

data Descr
    = DConst  !Name   !String !Type       [Tag]
    | DSymbol !Name   !String !Obligation [Tag]
    | DValue  !Name   !Type   !Obligation [Tag]
    | DType   !Type           !Obligation [Tag]
    | DDoc    !String
    | DLabel  !String !Descr
    | DEither !Descr  !Descr
    | DAll    !Descr  !Descr
    deriving (Eq, Show)

class Description a where
    describe :: a -> Descr

foldD :: (a -> Descr -> a) -> a -> Descr -> a
foldD f !z (DAll    !a !b) = foldD f (foldD f z a) b
foldD f !z (DEither !a !b) = foldD f (foldD f z a) b
foldD f !z !d              = f z d

mapD :: (Descr -> a) -> Descr -> [a]
mapD f = foldD (\a d -> f d : a) []

filterD :: (Descr -> Bool) -> Descr -> [Descr]
filterD f = foldD (\a d -> if f d then d : a else a) []

name :: Descr -> Maybe Name
name (DConst  n _ _ _)  = Just n
name (DSymbol n _ _ _)  = Just n
name (DValue  n _ _ _)  = Just n
name _                  = Nothing

obligation :: Descr -> Maybe Obligation
obligation (DSymbol _ _ o _) = Just o
obligation (DValue  _ _ o _) = Just o
obligation (DType   _ o _)   = Just o
obligation _                 = Nothing

typ :: Descr -> Maybe Type
typ (DConst  _ _ t _)    = Just t
typ (DValue  _ t _ _)    = Just t
typ (DType   t _ _)      = Just t
typ (DLabel  _ d)        = typ d
typ _                    = Nothing

tags :: Descr -> [Tag]
tags (DConst  _ _ _ t) = t
tags (DSymbol _ _ _ t) = t
tags (DValue  _ _ _ t) = t
tags (DType   _ _ t)   = t
tags (DLabel _ d)      = tags d
tags _                 = []

toList :: Descr -> [Descr]
toList d = go d []
  where
    go (DAll    !a !b) !acc = go a acc ++ go b acc
    go (DEither !a !b) !acc = go a acc ++ go b acc
    go !descr          !acc = descr : acc
