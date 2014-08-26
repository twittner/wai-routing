-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import Criterion
import Criterion.Main
import Network.HTTP.Types hiding (ok200)
import Network.Wai
import Network.Wai.Internal (ResponseReceived (..))
import Network.Wai.Predicate
import Network.Wai.Routing

sitemap :: Routes a IO ()
sitemap = do
    get "/a" (continue handlerA) (query "foo")
    get "/b" (continue handlerB) (query "foo" .&. query "bar")
    get "/c" (continue handlerC) (query "foo" .&. query "bar" .&. query "baz")
    get "/d" (continue handlerD) (query "foo" .&. query "bar" .&. query "baz" .&. query "zoo")
    get "/z" (continue handlerZ) $
          query "foo"
      .&. query "bar"
      .&. query "baz"
      .&. query "zoo"
      .&. query "x1"
      .&. query "x2"
      .&. query "x3"
      .&. query "x4"
      .&. query "x5"
      .&. query "x6"
      .&. query "x7"
      .&. query "x8"

handlerA :: Int -> IO Response
handlerA _ = return ok200

handlerB :: Int ::: Int -> IO Response
handlerB _ = return ok200

handlerC :: Int ::: Int ::: Int -> IO Response
handlerC _ = return ok200

handlerD :: Int ::: Int ::: Int ::: Int -> IO Response
handlerD _ = return ok200

handlerZ :: Int ::: Int ::: Int ::: Int ::: Int ::: Int ::: Int ::: Int ::: Int ::: Int ::: Int ::: Int -> IO Response
handlerZ _ = return ok200

ok200 :: Response
ok200 = responseLBS status200 [] ""

reqABad, reqBBad, reqCBad, reqDBad, reqZBad :: Request
reqABad = defaultRequest { rawPathInfo = "/a" }
reqBBad = reqAOk { rawPathInfo = "/b" }
reqCBad = reqBOk { rawPathInfo = "/c" }
reqDBad = reqCOk { rawPathInfo = "/d" }
reqZBad = defaultRequest
    { rawPathInfo = "/z"
    , queryString =
        [ ("foo", Just "42")
        , ("bar", Just "42")
        , ("naz", Just "42")
        , ("zoo", Just "42")
        , ("x1", Just "42")
        , ("x2", Just "42")
        , ("x3", Just "42")
        , ("x4", Just "42")
        , ("x5", Just "42")
        , ("x6", Just "42")
        , ("x7", Just "42")
        ]
    }

reqAOk, reqBOk, reqCOk, reqDOk, reqZOk :: Request
reqAOk = reqABad { queryString = [("foo", Just "42")] }
reqBOk = reqBBad { queryString = ("bar", Just "100") : queryString reqAOk }
reqCOk = reqCBad { queryString = ("baz", Just "0") : queryString reqBOk }
reqDOk = reqDBad { queryString = ("zoo", Just "1") : queryString reqCOk }
reqZOk = reqZBad { queryString = ("x8", Just "42") : queryString reqZBad }

main :: IO ()
main = defaultMain
    [ bgroup "bench"
        [ bench "a - ok"  (whnfIO $ f reqAOk)
        , bench "a - bad" (whnfIO $ f reqABad)
        , bench "b - ok"  (whnfIO $ f reqBOk)
        , bench "b - bad" (whnfIO $ f reqBBad)
        , bench "c - ok"  (whnfIO $ f reqCOk)
        , bench "c - bad" (whnfIO $ f reqCBad)
        , bench "d - ok"  (whnfIO $ f reqDOk)
        , bench "d - bad" (whnfIO $ f reqDBad)
        , bench "z - ok"  (whnfIO $ f reqZOk)
        , bench "z - bad" (whnfIO $ f reqZBad)
        ]
    ]
  where
    f rq = route (prepare sitemap) rq rs
    rs   = const (return ResponseReceived)
