-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module Network.Wai.Predicate.MediaType where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import GHC.TypeLits

data Media (t :: Symbol) (s :: Symbol) = Media
    { rawType      :: !ByteString
    , rawSubTypes  :: !ByteString
    , mediaQuality :: !Double
    , mediaParams  :: ![(ByteString, ByteString)]
    } deriving (Eq, Show)

mediaType :: SingI t => Media t s -> ByteString
mediaType m = withSing (f m)
  where
    f :: Media t s -> Sing t -> ByteString
    f _ t = pack (fromSing t)

mediaSubType :: SingI s => Media t s -> ByteString
mediaSubType m = withSing (f m)
  where
    f :: Media t s -> Sing s -> ByteString
    f _ s = pack (fromSing s)
