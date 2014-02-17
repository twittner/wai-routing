-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}

module Network.Wai.Routing.Route
    ( Routes
    , Renderer
    , route
    , expand
    , renderer
    , addRoute
    , get
    , Network.Wai.Routing.Route.head
    , post
    , put
    , delete
    , trace
    , options
    , connect
    ) where

import Control.Applicative hiding (Const)
import Control.Monad
import Control.Monad.Trans.State.Strict hiding (get, put)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.Either
import Data.Function
import Data.List hiding (head, delete)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Network.HTTP.Types
import Network.Wai (Request, Response, responseLBS, responseBuilder, rawPathInfo)
import Network.Wai.Routing.Predicate
import Network.Wai.Routing.Error
import Network.Wai.Routing.Request

import qualified Data.ByteString.Char8  as C
import qualified Data.ByteString.Lazy   as Lazy
import qualified Data.List              as L
import qualified Network.Wai.Route.Tree as Tree

data Route m = Route
    { _method  :: !Method
    , _path    :: !ByteString
    , _pred    :: Pack m
    }

data Handler m = Handler
    { _delta   :: !Delta
    , _handler :: m Response
    }

data Pack m where
    Pack :: (Predicate p Req, FVal p ~ Error)
         => p
         -> (TVal p -> m Response)
         -> Pack m

-- | Function to turn an 'Error' value into a 'Lazy.ByteString'.
-- Clients can provide their own renderer using 'renderer'.
type Renderer = Error -> Maybe Lazy.ByteString

-- | Set a custom render function, i.e. a function to turn 'Error's into
-- 'Lazy.ByteString's.
renderer :: Renderer -> Routes m ()
renderer f = Routes . modify $ \(St !rr _) -> St rr f

-- | The Routes monad state type.
data St m = St [Route m] Renderer

-- | Initial state.
zero :: St m
zero = St [] (fmap Lazy.fromStrict . message)

-- | The Routes monad is used to add routing declarations
-- via 'addRoute' or one of 'get', 'post', etc.
newtype Routes m a = Routes { _unroutes :: State (St m) a }

instance Functor (Routes m) where
    fmap = liftM

instance Applicative (Routes m) where
    pure  = return
    (<*>) = ap

instance Monad (Routes m) where
    return  = Routes . return
    m >>= f = Routes $ _unroutes m >>= _unroutes . f

-- | Add a route for some 'Method' and path (potentially with variable
-- captures) and constrained by some 'Predicate'.
addRoute :: (Monad m, Predicate p Req, FVal p ~ Error)
         => Method
         -> ByteString             -- ^ path
         -> (TVal p -> m Response) -- ^ handler
         -> p                      -- ^ 'Predicate'
         -> Routes m ()
addRoute m r x p = Routes . modify $ \(St !rr !f) ->
    St (Route m r (Pack p x) : rr) f

-- | Specialisation of 'addRoute' for a specific HTTP 'Method'.
get, head, post, put, delete, trace, options, connect ::
    (Monad m, Predicate p Req, FVal p ~ Error)
    => ByteString             -- ^ path
    -> (TVal p -> m Response) -- ^ handler
    -> p                      -- ^ 'Predicate'
    -> Routes m ()
get     = addRoute (renderStdMethod GET)
head    = addRoute (renderStdMethod HEAD)
post    = addRoute (renderStdMethod POST)
put     = addRoute (renderStdMethod PUT)
delete  = addRoute (renderStdMethod DELETE)
trace   = addRoute (renderStdMethod TRACE)
options = addRoute (renderStdMethod OPTIONS)
connect = addRoute (renderStdMethod CONNECT)

-- | A WAI 'Application' (generalised from 'IO' to 'Monad') which
-- routes requests to handlers based on predicated route declarations.
route :: Monad m => Routes m a -> Request -> m Response
route rm rq = do
    let tr = Tree.fromList $ expand rm
    case Tree.lookup tr (Tree.segments $ rawPathInfo rq) of
        Just (f, v) -> f (fromWaiRequest v rq)
        Nothing     -> return notFound
  where
    notFound = responseLBS status404 [] ""

-- | Run the 'Routes' monad and return the handlers per path.
expand :: Monad m => Routes m a -> [(ByteString, Req -> m Response)]
expand (Routes routes) =
    let St rr f = execState routes zero in
    map (\g -> (_path (L.head g), select f g)) (normalise rr)

-- | Group routes by path.
normalise :: [Route m] -> [[Route m]]
normalise rr =
    let rg    = grouped . sorted $ rr
        paths = map (namelessPath . L.head) rg
        ambig = paths \\ nub paths
    in if null ambig then rg else error (ambiguityMessage ambig)
  where
    sorted :: [Route m] -> [Route m]
    sorted = sortBy (compare `on` _path)

    grouped :: [Route m] -> [[Route m]]
    grouped = groupBy ((==) `on` _path)

    namelessPath :: Route m -> ByteString
    namelessPath =
        let fun s = if s /= "" && C.head s == ':' then "<>" else s
        in C.intercalate "/" . map fun . C.split '/' . _path

    ambiguityMessage a =
        "Paths differing only in variable names are not supported.\n"  ++
        "Problematic paths (with variable positions denoted by <>):\n" ++
        show a

-- The handler selection proceeds as follows:
-- (1) Consider only handlers with matching methods, or else return 405.
-- (2) Evaluate 'Route' predicates.
-- (3) Pick the first one which is 'Good', or else respond with status
--     and message of the first one.
select :: Monad m => Renderer -> [Route m] -> Req -> m Response
select render routes req = do
    let ms = filter ((method req ==) . _method) routes
    if null ms
        then return $ respond render (Error status405 Nothing) [(allow, validMethods)]
        else evalAll ms
  where
    allow :: HeaderName
    allow = mk "Allow"

    validMethods :: ByteString
    validMethods = C.intercalate "," $ nub (C.pack . show . _method <$> routes)

    evalAll :: Monad m => [Route m] -> m Response
    evalAll rs =
        let (n, y) = partitionEithers $ foldl' evalSingle [] rs
        in if null y
            then return $ respond render (L.head n) []
            else closest y

    evalSingle :: Monad m => [Either Error (Handler m)] -> Route m -> [Either Error (Handler m)]
    evalSingle rs r =
        case _pred r of
            Pack p h -> case apply p req of
                F   m -> Left m : rs
                T d v -> Right (Handler d (h v)) : rs

    closest :: Monad m => [Handler m] -> m Response
    closest hh = case map _handler . sortBy (compare `on` _delta) $ hh of
        []  -> return $ responseBuilder status404 [] mempty
        h:_ -> h

respond :: Renderer -> Error -> ResponseHeaders -> Response
respond f e h = responseLBS (status e) h (fromMaybe mempty (f e))
