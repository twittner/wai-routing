-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Network.Wai.Routing.Predicate.Content
    ( ContentType (..)
    , module Network.Wai.Routing.MediaType
    ) where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Monoid hiding (All)
import GHC.TypeLits
import Data.Maybe
import Network.HTTP.Types.Status
import Network.Wai.Routing.Error
import Network.Wai.Routing.MediaType
import Network.Wai.Routing.Predicate.Predicate
import Network.Wai.Routing.Request

import qualified Network.Wai.Routing.Parser.MediaType as M

-- | A 'Predicate' against the 'Request's \"Content-Type\" header.
data ContentType (t :: Symbol) (s :: Symbol) = ContentType

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

instance (Applicative m, SingI t, SingI s) => Predicate m (ContentType t s) Req where
    type FVal (ContentType t s) = Error
    type TVal (ContentType t s) = Media t s
    apply c r = pure $
        let mtypes = M.readMediaTypes "content-type" r in
        case findContentType c mtypes of
            m:_ -> T (1.0 - mediaQuality m) m
            []  -> F (err status415 msg)
      where
        msg = "Expected 'Content-Type: " <> type1 c <> "/" <> type2 c <> "'."

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
