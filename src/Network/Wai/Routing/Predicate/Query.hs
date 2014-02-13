-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Wai.Routing.Predicate.Query
    ( Query    (..)
    , HasQuery (..)
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.From
import Data.Monoid
import Network.HTTP.Types.Status
import Network.Wai.Routing.Error
import Network.Wai.Routing.Internal
import Network.Wai.Routing.Predicate.Predicate
import Network.Wai.Routing.Request

newtype Query a = Query ByteString

instance (FromByteString a) => Predicate (Query a) Request where
    type FVal (Query a) = Error
    type TVal (Query a) = a
    apply (Query x)     =
        let msg = "Missing query '" <> x <> "'." in
        rqApply (lookupQuery x) readValues (err status400 msg)

instance Show (Query a) where
    show (Query n) = "Query: " ++ show n

newtype HasQuery = HasQuery ByteString

instance Predicate HasQuery Request where
    type FVal HasQuery   = Error
    type TVal HasQuery   = ()
    apply (HasQuery x) r =
        if null (lookupQuery x r)
            then F (err status400 ("Missing query '" <> x <> "'."))
            else T 0 ()

instance Show HasQuery where
    show (HasQuery x) = "HasQuery: " ++ show x
