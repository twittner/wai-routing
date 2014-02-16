{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Tests.Wai.Predicate (tests) where

import Data.ByteString (ByteString)
import Network.HTTP.Types.Status
import Network.Wai.Routing
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Wai.Util

tests :: TestTree
tests = testGroup "Wai.Predicate"
    [ testCase "Accept application/json" testAcceptJson
    , testCase "Accept application/thrift " testAcceptThrift
    , testCase "Accept application/*" testAcceptAll
    , testCase "Content-Type text/plain" testContentTypePlain
    , testCase "Content-Type text/*" testContentTypeAll
    , testCase "Query" testQuery
    , testCase "QueryOpt" testQueryOpt
    ]

testAcceptJson :: IO ()
testAcceptJson = do
    let rq0 = fromWaiRequest [] . json $ request "/"
    x0 <- apply (Accept :: Accept "application" "json") rq0
    (T 0 $ Media "application" "json" 1.0 []) @=? x0

    let rq1 = fromWaiRequest [] . withHeader "Accept" "foo/bar" $ request "/"
    x1 <- apply (Accept :: Accept "application" "json") rq1
    (F (err status406 ("Expected 'Accept: application/json'."))) @=? x1

testAcceptThrift :: IO ()
testAcceptThrift = do
    let rq0 = fromWaiRequest [] . withHeader "Accept" "application/x-thrift" $ request "/"
    x0 <- apply (Accept :: Accept "application" "x-thrift") rq0
    (T 0 $ Media "application" "x-thrift" 1.0 []) @=? x0

    let rq1 = fromWaiRequest [] . json $ request "/"
    x1 <- apply (Accept :: Accept "application" "x-thrift") rq1
    (F (err status406 ("Expected 'Accept: application/x-thrift'."))) @=? x1

testAcceptAll :: IO ()
testAcceptAll = do
    let rq0 = fromWaiRequest [] . withHeader "Accept" "application/*" $ request "/"
    x0 <- apply (Accept :: Accept "application" "*") rq0
    (T 0 $ Media "application" "*"    1.0 []) @=? x0
    x1 <- apply (Accept :: Accept "application" "json") rq0
    (T 0 $ Media "application" "json" 1.0 []) @=? x1

testContentTypePlain :: IO ()
testContentTypePlain = do
    let rq0 = fromWaiRequest [] . withHeader "Content-Type" "text/plain" $ request "/"
    x0 <- apply (ContentType :: ContentType "text" "plain") rq0
    (T 0 $ Media "text" "plain" 1.0 []) @=? x0

    let rq1 = fromWaiRequest [] . withHeader "Content-Type" "text/html" $ request "/"
    x1 <- apply (ContentType :: ContentType "text" "plain") rq1
    (F (err status415 ("Expected 'Content-Type: text/plain'."))) @=? x1

testContentTypeAll :: IO ()
testContentTypeAll = do
    let rq0 = fromWaiRequest [] . withHeader "Content-Type" "text/plain" $ request "/"
    x0 <- apply (ContentType :: ContentType "text" "*") rq0
    (T 0.5 $ Media "text" "plain" 0.5 []) @=? x0

testQuery :: IO ()
testQuery = do
    let rq0 = fromWaiRequest [] . withQuery "x" "y" . withQuery "x" "z" $ request "/"
    x0 <- apply (Query "x" :: Query ByteString) rq0
    (T 0 "y") @=? x0

    let rq1 = fromWaiRequest [] $ request "/"
    x1 <- apply (Query "x" :: Query ByteString) rq1
    (F (err status400 ("Missing query 'x'."))) @=? x1

testQueryOpt :: IO ()
testQueryOpt = do
    let rq0 = fromWaiRequest [] . withQuery "x" "y" . withQuery "x" "z" $ request "/"
    x0 <- apply (Opt (Query "x" :: Query ByteString)) rq0
    (T 0 (Just "y")) @=? x0

    let rq1 = fromWaiRequest [] $ request "/"
    x1 <- apply (Opt (Query "x" :: Query ByteString)) rq1
    (T 0 Nothing) @=? x1

