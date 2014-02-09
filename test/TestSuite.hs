module Main where

import Test.Tasty
import qualified Tests.Data.Predicate as Predicate
import qualified Tests.Wai.Predicate  as WaiPredicate
import qualified Tests.Wai.Route      as WaiRoute

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Predicate.tests
    , WaiPredicate.tests
    , WaiRoute.tests
    ]
