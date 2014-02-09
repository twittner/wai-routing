-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Network.Wai.Predicate.Content
    ( ContentType
    , contentType
    , module Network.Wai.Predicate.MediaType
    ) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Monoid hiding (All)
import Data.Predicate
import Data.Predicate.Descr
import GHC.TypeLits
import Data.Maybe
import Network.HTTP.Types.Status
import Network.Wai.Predicate.Error
import Network.Wai.Predicate.MediaType
import Network.Wai.Predicate.Request

import qualified Network.Wai.Predicate.Parser.MediaType as M

-- | A 'Predicate' against the 'Request's \"Content-Type\" header.
data ContentType (t :: Symbol) (s :: Symbol) = ContentType

{-# INLINE contentType #-}
contentType :: ContentType t s
contentType = ContentType

type1 :: SingI t => ContentType t s -> ByteString
type1 m = withSing (f m)
  where
    f :: ContentType t s -> Sing t -> ByteString
    f _ t = pack $ fromSing t

type2 :: SingI s => ContentType t s -> ByteString
type2 m = withSing (f m)
  where
    f :: ContentType t s -> Sing s -> ByteString
    f _ s = pack $ fromSing s

instance (SingI t, SingI s) => Predicate (ContentType t s) Request where
    type FVal (ContentType t s) = Error
    type TVal (ContentType t s) = Media t s
    apply c r = let mtypes = M.readMediaTypes "content-type" r in
        case findContentType c mtypes of
            m:_ -> T (1.0 - mediaQuality m) m
            []  -> F (err status415 msg)
      where
        msg = "Expected 'Content-Type: " <> type1 c <> "/" <> type2 c <> "'."

instance (SingI t, SingI s) => Show (ContentType t s) where
    show c = unpack $ "ContentType: " <> type1 c <> "/" <> type2 c

instance (SingI t, SingI s) => Description (ContentType t s) where
    describe a = DSymbol "Content-Type" (show $ type1 a <> "/" <> type2 a) Required ["Header"]

findContentType :: (SingI t, SingI s) => ContentType t s -> [M.MediaType] -> [Media t s]
findContentType c = mapMaybe (\m -> do
    let ct = type1 c
        cs = type2 c
        mt = M.medType m
        ms = M.medSubtype m
    guard (ct == "*" || ct == mt && cs == "*" || cs == ms)
    return $ Media mt ms (quality ct cs) (M.medParams m))
  where
    quality "*" "*" = 0
    quality "*"  _  = 0.2
    quality  _  "*" = 0.5
    quality  _   _  = 1.0
