-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Wai.Predicate.Query
    ( RqQuery  (..)
    , Query    (..)
    , QueryOpt (..)
    , QueryDef (..)
    , HasQuery (..)
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Read
import Data.Monoid
import Data.Predicate
import Network.HTTP.Types.Status
import Network.Wai.Predicate.Error
import Network.Wai.Predicate.Internal
import Network.Wai.Predicate.Request

-- | The most generic request query predicate provided.
-- It will get all request query values of 'qName' and pass them on to
-- the conversion function 'qRead', which might either yield an error
-- message or a value. If the query is not present, an optional default may be
-- returned instead, if nothing is provided, the error message will be used
-- when construction the 400 status.
data RqQuery a = RqQuery
    { qName :: !ByteString
    , qRead :: [ByteString] -> Either ByteString a
    , qDef  :: Maybe a
    }

instance Predicate (RqQuery a) Request where
    type FVal (RqQuery a)     = Error
    type TVal (RqQuery a)     = a
    apply (RqQuery nme f def) =
        rqApply (lookupQuery nme)
                def
                f
                (err status400 ("Missing query '" <> nme <> "'."))

instance Show (RqQuery a) where
    show a = "RqQuery: " ++ show (qName a)

-- | Specialised query predicate which returns the first request
-- query which could be converted to the target type.
-- Relies on 'Readable' type-class for the actual conversion.
newtype Query a = Query ByteString

instance (Readable a) => Predicate (Query a) Request where
    type FVal (Query a) = Error
    type TVal (Query a) = a
    apply (Query x)     = apply (RqQuery x readValues Nothing)

instance Show (Query a) where
    show (Query n) = "Query: " ++ show n

-- | Specialised query predicate which returns the first request
-- query which could be converted to the target type.
-- If the query is not present, the provided default will be used.
-- Relies on 'Readable' type-class for the actual conversion.
data QueryDef a = QueryDef ByteString a

instance (Readable a) => Predicate (QueryDef a) Request where
    type FVal (QueryDef a) = Error
    type TVal (QueryDef a) = a
    apply (QueryDef x d)   = apply (RqQuery x readValues (Just d))

instance (Show a) => Show (QueryDef a) where
    show (QueryDef x d) = "QueryDef: " ++ show x ++ " [" ++ show d ++ "]"

-- | Predicate which returns the first request query which could be
-- converted to the target type wrapped in a Maybe.
-- If the query is not present, 'Nothing' will be returned.
-- Relies on 'Readable' type-class for the actual conversion.
newtype QueryOpt a = QueryOpt ByteString

instance (Readable a) => Predicate (QueryOpt a) Request where
    type FVal (QueryOpt a) = Error
    type TVal (QueryOpt a) = Maybe a
    apply (QueryOpt x)     = rqApplyMaybe (lookupQuery x) readValues

instance Show (QueryOpt a) where
    show (QueryOpt n) = "QueryOpt: " ++ show n

-- | Predicate which is true if the request has a query with the
-- given name.
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
