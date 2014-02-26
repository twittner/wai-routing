module Main where

import Test.Tasty
import qualified Tests.Wai.Route as WaiRoute

main :: IO ()
main = defaultMain $ testGroup "Tests" [ WaiRoute.tests ]
