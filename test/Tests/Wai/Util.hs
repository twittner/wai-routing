{-# LANGUAGE OverloadedStrings #-}

module Tests.Wai.Util where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.IORef
import Network.Wai
import Network.Wai.Internal
import Network.HTTP.Types

import qualified Data.ByteString.Lazy     as Lazy
import qualified Blaze.ByteString.Builder as Builder

request :: ByteString -> Request
request p = defaultRequest { rawPathInfo = p }

withHeader :: CI ByteString -> ByteString -> Request -> Request
withHeader k v r = r { requestHeaders = (k, v) : requestHeaders r }

withQuery :: ByteString -> ByteString -> Request -> Request
withQuery k v r = r { queryString = (k, Just v) : queryString r }

json :: Request -> Request
json = withHeader "Accept" "application/json"

responseBody :: Response -> Lazy.ByteString
responseBody (ResponseBuilder _ _ b) = Builder.toLazyByteString b
responseBody _                       = ""

writeText :: Lazy.ByteString -> IO Response
writeText = return . responseLBS status200 []

apply :: (a -> (Response -> IO ResponseReceived) -> IO ResponseReceived) -> a -> IO Response
apply f r = do
    ref <- newIORef undefined
    void $ f r $ \x -> writeIORef ref x >> return ResponseReceived
    readIORef ref
