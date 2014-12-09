-- A contrived WAI application which does not make use of
-- wai-routing, but instead works directly on WAI Request.
-- It exposes a single route "eval" to evaluate a small
-- expression which is passed in as query parameters.
--
-- You need wai-extra whic containes the RequestLogger to
-- compile this example.
--
-- The file "eval-routing.hs" does the same but using wai-routing.

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.ByteString.Conversion
import Data.String
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger

import qualified Data.ByteString.Lazy as Lazy

-- The operations we want to support in expressions.
data Op = Add | Sub | Mul | Div

instance FromByteString Op where
    parser = anyChar >>= \c -> case c of
        '+' -> return Add
        '-' -> return Sub
        '*' -> return Mul
        '/' -> return Div
        _   -> fail $ "Invalid operation: " ++ show c

main :: IO ()
main = run 8080 $ logStdout start

start :: Application
start r k = case pathInfo r of
    "eval":[] ->
        let x = join $ lookup "x" (queryString r)
            y = join $ lookup "y" (queryString r)
            f = join $ lookup "f" (queryString r)
        in either (k . respond status400 . fromString)
                  (k . respond status200 . fromString . show)
                  (eval f x y)
    _ -> k $ respond status404 ""

eval :: Maybe ByteString -> Maybe ByteString -> Maybe ByteString -> Either String Int
eval (Just f) (Just x) (Just y) = do
    x' <- parseOnly parser x
    y' <- parseOnly parser y
    f' <- parseOnly parser f
    case f' of
        Add -> return (x' + y')
        Sub -> return (x' - y')
        Mul -> return (x' * y')
        Div -> return (x' `div` y')
eval _ _ _ = Left "invalid arguments"

respond :: Status -> Lazy.ByteString -> Response
respond s = responseLBS s []
