-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Network.Wai.Routing.Predicate.Predicate
    ( Delta
    , Boolean   (..)
    , Predicate (..)
    , (:|:)     (..)
    , (:&:)     (..)
    , (:||:)    (..)
    , (:::)     (..)
    , (:+:)
    , Const
    , Fail
    , Opt
    , Def
    , PMap
    , PMapT
    , PMapF

    , constant
    , failure
    , true
    , opt
    , def
    , pmap
    , pmapT
    , pmapF
    , with
    ) where

import Prelude hiding (and, or)

-- | 'Delta' is a measure of distance. It is (optionally)
-- used in predicates that evaluate to 'T' but not uniquely so, i.e.
-- different evaluations of 'T' are possible and they may have a different
-- \"fitness\".
--
-- An example is content-negotiation. A HTTP request may specify
-- a preference list of various media-types. A predicate matching one
-- specific media-type evaluates to 'T', but other media-types may match
-- even better. To represent this ambivalence, the predicate will include
-- a delta value which can be used to decide which of the matching
-- predicates should be preferred.
type Delta = Double

-- | A 'Bool'-like type where each branch 'T'rue or 'F'alse carries
-- some meta-data which is threaded through 'Predicate' evaluation.
data Boolean f t
    = F f       -- ^ logical False with some meta-data
    | T Delta t -- ^ logical True with some meta-data
    deriving (Eq, Show)

-- | The 'Predicate' class declares the function 'apply' which
-- evaluates the predicate against some value, returning a value
-- of type 'Boolean'.
-- Besides being parameterised over predicate type and predicate
-- parameter, the class is also parameterised over the actual types
-- of T's and F's meta-data.
class Predicate p a where
    type FVal p
    type TVal p
    apply :: p -> a -> Boolean (FVal p) (TVal p)

-- | A 'Predicate' instance which always returns 'T' with
-- the given value as T's meta-data.
data Const f t where
    Const :: t -> Const f t

instance Predicate (Const f t) a where
    type FVal (Const f t) = f
    type TVal (Const f t) = t
    apply (Const a) _     = T 0 a

constant :: t -> Const f t
constant = Const
{-# INLINABLE constant #-}

true :: Const a ()
true = Const ()
{-# INLINABLE true #-}

-- | A 'Predicate' instance which always returns 'F' with
-- the given value as F's meta-data.
data Fail f t where
    Fail :: f -> Fail f t

failure :: f -> Fail f t
failure = Fail
{-# INLINABLE failure #-}

instance Predicate (Fail f t) a where
    type FVal (Fail f t) = f
    type TVal (Fail f t) = t
    apply (Fail a) _     = F a

-- | A 'Predicate' instance corresponding to the logical
-- OR connective of two 'Predicate's. It requires the
-- meta-data of each 'T'rue branch to be of the same type.
--
-- If both arguments evaluate to 'T' the one with the
-- smaller 'Delta' will be preferred, or--if equal--the
-- left-hand argument.
data a :|: b = a :|: b

instance (Predicate a c, Predicate b c, TVal a ~ TVal b, FVal a ~ FVal b) => Predicate (a :|: b) c
  where
    type FVal (a :|: b) = FVal a
    type TVal (a :|: b) = TVal a
    apply (a :|: b) r   = apply a r `or` apply b r
      where
        or x@(T d0 _) y@(T d1 _) = if d1 < d0 then y else x
        or x@(T _  _)   (F    _) = x
        or (F      _) x@(T _  _) = x
        or (F      _) x@(F    _) = x

type a :+: b = Either a b

-- | A 'Predicate' instance corresponding to the logical
-- OR connective of two 'Predicate's. The meta-data of
-- each 'T'rue branch can be of different types.
--
-- If both arguments evaluate to 'T' the one with the
-- smaller 'Delta' will be preferred, or--if equal--the
-- left-hand argument.
data a :||: b = a :||: b

instance (Predicate a c, Predicate b c, FVal a ~ FVal b) => Predicate (a :||: b) c
  where
    type FVal (a :||: b) = FVal a
    type TVal (a :||: b) = TVal a :+: TVal b
    apply (a :||: b) r   = apply a r `or` apply b r
      where
        or (T d0 t0) (T d1 t1) = if d1 < d0 then T d1 (Right t1) else T d0 (Left t0)
        or (T  d  t) (F     _) = T d (Left t)
        or (F     _) (T  d  t) = T d (Right t)
        or (F     _) (F     f) = F f

-- | Data-type used for tupling-up the results of ':&:'.
data a ::: b = a ::: b deriving (Eq, Show)

-- | A 'Predicate' instance corresponding to the logical
-- AND connective of two 'Predicate's.
data a :&: b = a :&: b

instance (Predicate a c, Predicate b c, FVal a ~ FVal b) => Predicate (a :&: b) c
  where
    type FVal (a :&: b) = FVal a
    type TVal (a :&: b) = TVal a ::: TVal b
    apply (a :&: b) r   = apply a r `and` apply b r
      where
        and (T d x) (T w y) = T (d + w) (x ::: y)
        and (T _ _) (F   f) = F f
        and (F   f) _       = F f

-- | A 'Predicate' modifier which makes the underlying predicate optional,
-- i.e. the 'TVal' becomes a 'Maybe' and in the failure-case 'Nothing' is
-- returned.
newtype Opt a = Opt a

opt :: a -> Opt a
opt = Opt
{-# INLINABLE opt #-}

instance (Predicate a b) => Predicate (Opt a) b where
    type FVal (Opt a) = FVal a
    type TVal (Opt a) = Maybe (TVal a)
    apply (Opt a) r   = case apply a r of
        T d x -> T d (Just x)
        F _   -> T 0 Nothing

-- | A 'Predicate' modifier which returns as 'TVal' the provided default
-- value if the underlying predicate fails.
data Def d a = Def d a

def :: d -> a -> Def d a
def = Def
{-# INLINABLE def #-}

instance (Predicate a b, d ~ TVal a) => Predicate (Def d a) b where
    type FVal (Def d a) = FVal a
    type TVal (Def d a) = TVal a
    apply (Def d a) r   = case apply a r of
        T n x -> T n x
        F _   -> T 0 d

-- | A 'Predicate' function, i.e. a function of the underlying predicate's
-- result.
data PMap a f t = PMap (Boolean (FVal a) (TVal a) -> Boolean f t) a

pmap :: (Boolean (FVal a) (TVal a) -> Boolean f t) -> a -> PMap a f t
pmap = PMap
{-# INLINABLE pmap #-}

instance (Predicate a b) => Predicate (PMap a f t) b where
    type FVal (PMap a f t) = f
    type TVal (PMap a f t) = t
    apply (PMap f a) r     = f $ apply a r

-- | Like 'PMap' but a function of the underlying predicate's 'TVal'.
data PMapT a t = PMapT (TVal a -> Boolean (FVal a) t) a

pmapT :: (TVal a -> Boolean (FVal a) t) -> a -> PMapT a t
pmapT = PMapT
{-# INLINABLE pmapT #-}

instance (Predicate a b) => Predicate (PMapT a t) b where
    type FVal (PMapT a t) = FVal a
    type TVal (PMapT a t) = t
    apply (PMapT f a) r   = case apply a r of
        (T _ x) -> f x
        (F x)   -> F x

-- | Like 'PMap' but a function of the underlying predicate's 'FVal'.
data PMapF a f = PMapF (FVal a -> Boolean f (TVal a)) a

pmapF :: (FVal a -> Boolean f (TVal a)) -> a -> PMapF a f
pmapF = PMapF
{-# INLINABLE pmapF #-}

instance (Predicate a b) => Predicate (PMapF a f) b where
    type FVal (PMapF a f) = f
    type TVal (PMapF a f) = TVal a
    apply (PMapF f a) r   = case apply a r of
        (F   x) -> f x
        (T d x) -> T d x

-- | The 'with' function will invoke the given function only if the predicate 'p'
-- applied to the test value 'a' evaluates to 'T'.
with :: (Monad m, Predicate p a) => p -> a -> (TVal p -> m ()) -> m ()
with p a f = case apply p a of
    T _ x -> f x
    _     -> return ()
