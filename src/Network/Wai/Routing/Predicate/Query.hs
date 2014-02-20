-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Wai.Routing.Predicate.Query
    ( Query
    , HasQuery
    , query
    , hasQuery
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

query :: ByteString -> Query a
query = Query
{-# INLINABLE query #-}

instance (FromByteString a) => Predicate (Query a) Req where
    type FVal (Query a) = Error
    type TVal (Query a) = a
    apply (Query x)     =
        let msg = "Missing query '" <> x <> "'." in
        rqApply (lookupQuery x) readValues (err status400 msg)

newtype HasQuery = HasQuery ByteString

hasQuery :: ByteString -> HasQuery
hasQuery = HasQuery
{-# INLINABLE hasQuery #-}

instance Predicate HasQuery Req where
    type FVal HasQuery   = Error
    type TVal HasQuery   = ()
    apply (HasQuery x) r =
        if null (lookupQuery x r)
            then F (err status400 ("Missing query '" <> x <> "'."))
            else T 0 ()
