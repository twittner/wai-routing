-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Predicate.Parser.Shared where

import Control.Applicative
import Data.Attoparsec.Char8
import Data.ByteString (ByteString)

import qualified Data.ByteString.Char8 as C

spaces :: Parser ()
spaces = skipWhile (== ' ')

trim :: Parser a -> Parser a
trim p = spaces *> p <* spaces

oneof :: ByteString -> Char -> Bool
oneof s c = C.any (== c) s
