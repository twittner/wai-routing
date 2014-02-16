-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Wai.Routing.Predicate.Query
    ( Query    (..)
    , HasQuery (..)
    ) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.ByteString.From
import Data.Monoid
import Network.HTTP.Types.Status
import Network.Wai.Routing.Error
import Network.Wai.Routing.Internal
import Network.Wai.Routing.Predicate.Predicate
import Network.Wai.Routing.Request

newtype Query a = Query ByteString

instance (Applicative m, FromByteString a) => Predicate m (Query a) Req where
    type FVal (Query a) = Error
    type TVal (Query a) = a
    apply (Query x)     =
        let msg = "Missing query '" <> x <> "'." in
        pure . rqApply (lookupQuery x) readValues (err status400 msg)

newtype HasQuery = HasQuery ByteString

instance Applicative m => Predicate m HasQuery Req where
    type FVal HasQuery   = Error
    type TVal HasQuery   = ()
    apply (HasQuery x) r = pure $
        if null (lookupQuery x r)
            then F (err status400 ("Missing query '" <> x <> "'."))
            else T 0 ()
