-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Wai.Predicate.Query
    ( Query
    , QueryOpt
    , QueryDef
    , HasQuery

    , query
    , queryOpt
    , queryDef
    , hasQuery
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Readable
import Data.Monoid
import Data.Predicate.Typeof
import Data.Proxy
import Data.Predicate
import Data.Predicate.Descr hiding (tags)
import Network.HTTP.Types.Status
import Network.Wai.Predicate.Error
import Network.Wai.Predicate.Internal
import Network.Wai.Predicate.Request

-- | The most generic request query predicate provided.
-- It will get all request query values of '_name' and pass them on to
-- the conversion function '_read', which might either yield an error
-- message or a value. If the query is not present, an optional default may be
-- returned instead, if nothing is provided, the error message will be used
-- when construction the 400 status.
data RqQuery a = RqQuery
    { _pName    :: !ByteString
    , _pRead    :: [ByteString] -> Either ByteString a
    , _pDefault :: Maybe a
    , _pProxy   :: Proxy a
    }

{-# INLINE rqQuery #-}
rqQuery :: ByteString
        -- ^ request rqQuery name
        -> ([ByteString] -> Either ByteString a)
        -- ^ conversion function
        -> Maybe a
        -- ^ optional default value
        -> RqQuery a
rqQuery n r d = RqQuery n r d Proxy

instance Predicate (RqQuery a) Request where
    type FVal (RqQuery a)       = Error
    type TVal (RqQuery a)       = a
    apply (RqQuery nme f def _) =
        rqApply (lookupQuery nme)
                def
                f
                (err status400 ("Missing query '" <> nme <> "'."))

instance (Typeof a) => Show (RqQuery a) where
    show (RqQuery n _ _ x) =
        "RqQuery: " ++ show n ++ " :: " ++ show (typeof x)

instance (Show a, Typeof a) => Description (RqQuery a) where
    describe (RqQuery n _ d x) =
        DValue (unpack n) (typeof x) (maybe Required (Default . show) d) tags

-- | Specialised query predicate which returns the first request
-- query which could be converted to the target type.
-- Relies on 'Readable' type-class for the actual conversion.
data Query a = Query ByteString (Proxy a)

{-# INLINE query #-}
query :: ByteString -> Query a
query n = Query n Proxy

instance (Readable a) => Predicate (Query a) Request where
    type FVal (Query a) = Error
    type TVal (Query a) = a
    apply (Query x _)   = apply (rqQuery x readValues Nothing)

instance (Typeof a) => Show (Query a) where
    show (Query n x) = "Query: " ++ show n ++ " :: " ++ show (typeof x)

instance (Typeof a) => Description (Query a) where
    describe (Query n x) = DValue (unpack n) (typeof x) Required tags

-- | Specialised query predicate which returns the first request
-- query which could be converted to the target type.
-- If the query is not present, the provided default will be used.
-- Relies on 'Readable' type-class for the actual conversion.
data QueryDef a = QueryDef ByteString a

{-# INLINE queryDef #-}
queryDef :: ByteString -> a -> QueryDef a
queryDef = QueryDef

instance (Readable a) => Predicate (QueryDef a) Request where
    type FVal (QueryDef a) = Error
    type TVal (QueryDef a) = a
    apply (QueryDef x d)   = apply (rqQuery x readValues (Just d))

instance (Show a, Typeof a) => Show (QueryDef a) where
    show (QueryDef x d) =
        "QueryDef: " ++ show x ++ " [" ++ show d ++ "] :: " ++ show (typeof d)

instance (Show a, Typeof a) => Description (QueryDef a) where
    describe (QueryDef n x) = DValue (unpack n) (typeof x) (Default (show x)) tags

-- | Predicate which returns the first request query which could be
-- converted to the target type wrapped in a Maybe.
-- If the query is not present, 'Nothing' will be returned.
-- Relies on 'Readable' type-class for the actual conversion.
data QueryOpt a = QueryOpt ByteString (Proxy a)

{-# INLINE queryOpt #-}
queryOpt :: ByteString -> QueryOpt a
queryOpt n = QueryOpt n Proxy

instance (Readable a) => Predicate (QueryOpt a) Request where
    type FVal (QueryOpt a) = Error
    type TVal (QueryOpt a) = Maybe a
    apply (QueryOpt x _)   = rqApplyMaybe (lookupQuery x) readValues

instance (Typeof a) => Show (QueryOpt a) where
    show (QueryOpt n x) = "QueryOpt: " ++ show n ++ " :: " ++ show (typeof x)

instance (Typeof a) => Description (QueryOpt a) where
    describe (QueryOpt n x) = DValue (unpack n) (typeof x) Optional tags

-- | Predicate which is true if the request has a query with the
-- given name.
data HasQuery = HasQuery ByteString

{-# INLINE hasQuery #-}
hasQuery :: ByteString -> HasQuery
hasQuery = HasQuery

instance Predicate HasQuery Request where
    type FVal HasQuery   = Error
    type TVal HasQuery   = ()
    apply (HasQuery x) r =
        if null (lookupQuery x r)
            then F (err status400 ("Missing query '" <> x <> "'."))
            else T 0 ()

instance Show HasQuery where
    show (HasQuery x) = "HasQuery: " ++ show x

instance Description HasQuery where
    describe (HasQuery n) = DValue (unpack n) (TName "()") Required tags

tags :: [Tag]
tags = ["Query"]

