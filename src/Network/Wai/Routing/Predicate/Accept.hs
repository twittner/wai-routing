-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Network.Wai.Routing.Predicate.Accept
  ( Accept (..)
  , module Network.Wai.Routing.MediaType
  )
where

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Monoid hiding (All)
import GHC.TypeLits
import Data.Maybe
import Network.HTTP.Types
import Network.Wai.Routing.Error
import Network.Wai.Routing.Predicate.Predicate
import Network.Wai.Routing.Request
import Network.Wai.Routing.MediaType

import qualified Network.Wai.Routing.Parser.MediaType as M

-- | A 'Predicate' against the 'Request's \"Accept\" header.
data Accept (t :: Symbol) (s :: Symbol) = Accept

type1 :: SingI t => Accept t s -> ByteString
type1 m = withSing (f m)
  where
    f :: Accept t s -> Sing t -> ByteString
    f _ t = pack $ fromSing t

type2 :: SingI s => Accept t s -> ByteString
type2 m = withSing (f m)
  where
    f :: Accept t s -> Sing s -> ByteString
    f _ s = pack $ fromSing s

instance (SingI t, SingI s) => Predicate (Accept t s) Request where
    type FVal (Accept t s) = Error
    type TVal (Accept t s) = Media t s
    apply a r = let mtypes = M.readMediaTypes "accept" r in
        if null mtypes
            then T 0 (Media (type1 a) (type2 a) 1.0 [])
            else case findMediaType a mtypes of
                m:_ -> T (1.0 - mediaQuality m) m
                []  -> F (err status406 msg)
      where
        msg = "Expected 'Accept: " <> type1 a <> "/" <> type2 a <> "'."

instance (SingI t, SingI s) => Show (Accept t s) where
    show a = unpack $ "Accept: " <> type1 a <> "/" <> type2 a

findMediaType :: (SingI t, SingI s) => Accept t s -> [M.MediaType] -> [Media t s]
findMediaType a = mapMaybe (\m -> do
    let at = type1 a
        as = type2 a
        mt = M.medType m
        ms = M.medSubtype m
    guard (mt == "*" || at == mt && ms == "*" || as == ms)
    return $ Media at as (M.medQuality m) (M.medParams m))
