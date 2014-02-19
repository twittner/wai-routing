{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- Like "direct.hs" but makes use of wai-routing.

module Main (main) where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.From
import Data.String
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Routing
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
main = run 8080 $ logStdout (route (prepare start))

start :: Monad m => Routes a m ()
start = get "eval" eval (Query "x" :&: Query "y" :&: Query "f")

eval :: Monad m => Int ::: Int ::: Op -> m Response
eval (x ::: y ::: f) = respond status200 . fromString . show $
    case f of
        Add -> x + y
        Sub -> x - y
        Mul -> x * y
        Div -> x `div` y

respond :: Monad m => Status -> Lazy.ByteString -> m Response
respond s = return . responseLBS s []
