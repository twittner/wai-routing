{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Tests.Data.Predicate (tests) where

import Control.Applicative hiding (Const, empty)
import Network.Wai.Routing.Predicate.Predicate
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Data.Predicate"
    [ testProperty "(:&:)"  (runProp testAnd)
    , testProperty "(:||:)" (runProp testOr)
    , testProperty "(:|:)"  (runProp testOr')
    ]

runProp :: (Show a, Show b, Arbitrary a, Arbitrary b) => (a -> b -> IO Bool) -> Property
runProp f = monadicIO $ do
    x <- pick arbitrary
    y <- pick arbitrary
    assert =<< run (f x y)

testAnd :: Applicative m => Rand -> Rand -> m Bool
testAnd a@(Rand (T d x)) b@(Rand (T w y)) = (==) <$> apply (a :&: b) () <*> pure (T (d + w) (x ::: y))
testAnd a@(Rand (T _ _)) b@(Rand (F   y)) = (==) <$> apply (a :&: b) () <*> pure (F y)
testAnd a@(Rand (F   x)) b@(Rand (T _ _)) = (==) <$> apply (a :&: b) () <*> pure (F x)
testAnd a@(Rand (F   x)) b@(Rand (F   _)) = (==) <$> apply (a :&: b) () <*> pure (F x)

testOr :: Applicative m => Rand -> Rand -> m Bool
testOr a@(Rand (T d x)) b@(Rand (T e y)) = (==) <$> apply (a :||: b) () <*> pure (if d <= e then T d (Left x) else T e (Right y))
testOr a@(Rand (T d x)) b@(Rand (F   _)) = (==) <$> apply (a :||: b) () <*> pure (T d (Left x))
testOr a@(Rand (F   _)) b@(Rand (T d y)) = (==) <$> apply (a :||: b) () <*> pure (T d (Right y))
testOr a@(Rand (F   _)) b@(Rand (F   y)) = (==) <$> apply (a :||: b) () <*> pure (F y)

testOr' :: Applicative m => Rand -> Rand -> m Bool
testOr' a@(Rand (T d x)) b@(Rand (T e y)) = (==) <$> apply (a :|: b) () <*> pure (if d <= e then T d x else T e y)
testOr' a@(Rand (T d x)) b@(Rand (F   _)) = (==) <$> apply (a :|: b) () <*> pure (T d x)
testOr' a@(Rand (F   _)) b@(Rand (T d y)) = (==) <$> apply (a :|: b) () <*> pure (T d y)
testOr' a@(Rand (F   _)) b@(Rand (F   y)) = (==) <$> apply (a :|: b) () <*> pure (F y)

newtype Rand = Rand
    { _rand :: Boolean Int Char
    } deriving Show

instance Applicative m => Predicate m Rand a where
    type FVal Rand   = Int
    type TVal Rand   = Char
    apply (Rand x) _ = pure x

instance Arbitrary (Boolean Int Char) where
    arbitrary =
        oneof [ T <$> (arbitrary :: Gen Delta) <*> (arbitrary :: Gen Char)
              , F <$> (arbitrary :: Gen Int)
              ]

instance Arbitrary Rand where
    arbitrary = Rand <$> (arbitrary :: Gen (Boolean Int Char))
