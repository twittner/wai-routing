-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Wai.Predicate.Types ( CSV, list ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8 (takeByteString)
import Data.ByteString (ByteString)
import Data.ByteString.Readable
import Data.Monoid
import Data.Predicate.Typeof
import GHC.Generics

import qualified Data.ByteString.Char8 as C

newtype CSV a = CSV
    { list :: [a]
    } deriving ( Eq
               , Ord
               , Read
               , Show
               , Functor
               , Generic
               , Monad
               , MonadPlus
               , Applicative
               , Alternative
               , Monoid )

instance Readable a => Readable (CSV a) where
    readByteString = do
        s <- takeByteString
        if C.null s
            then return empty
            else case mapM (fromByteString . trim) (C.split ',' s) of
                     Nothing -> fail "no parse"
                     Just xs -> return (CSV xs)

instance (Typeof a) => Typeof (CSV a)

trim :: ByteString -> ByteString
trim = fst . C.spanEnd (== ' ') . C.dropWhile (== ' ')
