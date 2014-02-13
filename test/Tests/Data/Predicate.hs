{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Tests.Data.Predicate (tests) where

import Control.Applicative hiding (Const, empty)
import Network.Wai.Routing.Predicate.Predicate
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Data.Predicate"
    [ testProperty "Const" testConst
    , testProperty "Fail" testFail
    , testProperty "(:&:)" testAnd
    , testProperty "(:||:)" testOr
    , testProperty "(:|:)" testOr'
    ]

testConst :: Const Int Char -> Bool
testConst x@(Const c) = apply x () == T 0 c

testFail :: Fail Int Char -> Bool
testFail x@(Fail c) = apply x () == F c

testAnd :: Rand -> Rand -> Bool
testAnd a@(Rand (T d x)) b@(Rand (T w y)) = apply (a :&: b) () == T (d + w) (x ::: y)
testAnd a@(Rand (T _ _)) b@(Rand (F   y)) = apply (a :&: b) () == F y
testAnd a@(Rand (F   x)) b@(Rand (T _ _)) = apply (a :&: b) () == F x
testAnd a@(Rand (F   x)) b@(Rand (F   _)) = apply (a :&: b) () == F x

testOr :: Rand -> Rand -> Bool
testOr a@(Rand (T d x)) b@(Rand (T e y)) = apply (a :||: b) () == if d <= e then T d (Left x) else T e (Right y)
testOr a@(Rand (T d x)) b@(Rand (F   _)) = apply (a :||: b) () == T d (Left x)
testOr a@(Rand (F   _)) b@(Rand (T d y)) = apply (a :||: b) () == T d (Right y)
testOr a@(Rand (F   _)) b@(Rand (F   y)) = apply (a :||: b) () == F y

testOr' :: Rand -> Rand -> Bool
testOr' a@(Rand (T d x)) b@(Rand (T e y)) = apply (a :|: b) () == if d <= e then T d x else T e y
testOr' a@(Rand (T d x)) b@(Rand (F   _)) = apply (a :|: b) () == T d x
testOr' a@(Rand (F   _)) b@(Rand (T d y)) = apply (a :|: b) () == T d y
testOr' a@(Rand (F   _)) b@(Rand (F   y)) = apply (a :|: b) () == F y

newtype Rand = Rand
  { _rand :: Boolean Int Char
  } deriving Show

instance Predicate Rand a where
    type FVal Rand   = Int
    type TVal Rand   = Char
    apply (Rand x) _ = x

instance Arbitrary (Boolean Int Char) where
    arbitrary =
        oneof [ T <$> (arbitrary :: Gen Delta) <*> (arbitrary :: Gen Char)
              , F <$> (arbitrary :: Gen Int)
              ]

instance Arbitrary (Const Int Char) where
    arbitrary = Const <$> (arbitrary :: Gen Char)

instance Arbitrary (Fail Int Char) where
    arbitrary = Fail <$> (arbitrary :: Gen Int)

instance Arbitrary Rand where
    arbitrary = Rand <$> (arbitrary :: Gen (Boolean Int Char))
