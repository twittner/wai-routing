-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Network.Wai.Routing.Internal
    ( readValues
    , rqApply
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.From
import Data.List (foldl')
import Data.String (fromString)
import Network.HTTP.Types
import Network.Wai.Routing.Error
import Network.Wai.Routing.Predicate.Predicate
import Network.Wai.Routing.Request

readValues :: FromByteString a => [ByteString] -> Either ByteString a
readValues = foldl' result (Left "no parse") . map (runParser parser)
  where
    result (Left  _) (Right x) = Right x
    result (Right x) _         = Right x
    result _         (Left  x) = Left (fromString x)

rqApply :: (Req -> [ByteString])
        -> ([ByteString] -> Either ByteString a)
        -> Error
        -> Req
        -> Boolean Error a
rqApply f reader e r =
    case f r of
        [] -> F e
        vs -> either (F . err status400) (T 0) $ reader vs
