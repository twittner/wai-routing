-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}

module Network.Wai.Routing.Route
    ( Routes
    , Renderer
    , route
    , prepare
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
    , attach
    , examine
    ) where

import Control.Applicative hiding (Const)
import Control.Monad
import Control.Monad.Trans.State.Strict hiding (get, put)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.Either
import Data.Function
import Data.List hiding (head, delete)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid
import Network.HTTP.Types
import Network.Wai (Request, Response, responseLBS, responseBuilder, rawPathInfo)
import Network.Wai.Predicate
import Network.Wai.Predicate.Request
import Network.Wai.Routing.Request

import qualified Data.ByteString.Char8  as C
import qualified Data.ByteString.Lazy   as Lazy
import qualified Data.List              as L
import qualified Network.Wai.Route.Tree as Tree

data Route a m = Route
    { _method  :: !Method
    , _path    :: !ByteString
    , _meta    :: Maybe a
    , _pred    :: Pack m
    }

data Handler m = Handler
    { _delta   :: !Double
    , _handler :: m Response
    }

data Pack m where
    Pack :: Predicate RoutingReq Error a -> (a -> m Response) -> Pack m

-- | Function to turn an 'Error' value into a 'Lazy.ByteString'.
-- Clients can provide their own renderer using 'renderer'.
type Renderer = Error -> Maybe Lazy.ByteString

-- | Set a custom render function, i.e. a function to turn 'Error's into
-- 'Lazy.ByteString's.
renderer :: Renderer -> Routes a m ()
renderer f = Routes . modify $ \s -> s { renderfn = f }

-- | The Routes monad state type.
data St a m = St
    { routes   :: [Route a m]
    , renderfn :: Renderer
    }

-- | Initial state.
zero :: St a m
zero = St [] (fmap Lazy.fromStrict . message)

-- | The Routes monad is used to add routing declarations
-- via 'addRoute' or one of 'get', 'post', etc.
newtype Routes a m b = Routes { _unroutes :: State (St a m) b }

instance Functor (Routes a m) where
    fmap = liftM

instance Applicative (Routes a m) where
    pure  = return
    (<*>) = ap

instance Monad (Routes a m) where
    return  = Routes . return
    m >>= f = Routes $ _unroutes m >>= _unroutes . f

-- | Add a route for some 'Method' and path (potentially with variable
-- captures) and constrained by some 'Predicate'.
addRoute :: Monad m
         => Method
         -> ByteString                   -- ^ path
         -> (a -> m Response)            -- ^ handler
         -> Predicate RoutingReq Error a -- ^ 'Predicate'
         -> Routes b m ()
addRoute m r x p = Routes . modify $ \s ->
    s { routes = Route m r Nothing (Pack p x) : routes s }

-- | Specialisation of 'addRoute' for a specific HTTP 'Method'.
get, head, post, put, delete, trace, options, connect ::
    Monad m
    => ByteString                   -- ^ path
    -> (a -> m Response)            -- ^ handler
    -> Predicate RoutingReq Error a -- ^ 'Predicate'
    -> Routes b m ()
get     = addRoute (renderStdMethod GET)
head    = addRoute (renderStdMethod HEAD)
post    = addRoute (renderStdMethod POST)
put     = addRoute (renderStdMethod PUT)
delete  = addRoute (renderStdMethod DELETE)
trace   = addRoute (renderStdMethod TRACE)
options = addRoute (renderStdMethod OPTIONS)
connect = addRoute (renderStdMethod CONNECT)

-- | Add some metadata to the last route.
attach :: a -> Routes a m ()
attach a = Routes $ modify addToLast
  where
    addToLast s@(St []   _) = s
    addToLast (St (r:rr) f) = St (r { _meta = Just a } : rr) f

-- | Get back all attached metadata.
examine :: Routes a m b -> [a]
examine (Routes r) = let St rr _ = execState r zero in
    mapMaybe _meta rr

-- | A WAI 'Application' (generalised from 'IO' to 'Monad') which
-- routes requests to handlers based on predicated route declarations.
route :: Monad m => [(ByteString, RoutingReq -> m Response)] -> Request -> m Response
route rm rq = do
    let tr = Tree.fromList rm
    case Tree.lookup tr (Tree.segments $ rawPathInfo rq) of
        Just (f, v) -> f (fromReq v (fromRequest rq))
        Nothing     -> return notFound
  where
    notFound = responseLBS status404 [] ""

-- | Run the 'Routes' monad and return the handlers per path.
prepare :: Monad m => Routes a m b -> [(ByteString, RoutingReq -> m Response)]
prepare (Routes rr) =
    let s = execState rr zero in
    map (\g -> (_path (L.head g), select (renderfn s) g)) (normalise (routes s))

-- | Group routes by path.
normalise :: [Route a m] -> [[Route a m]]
normalise rr =
    let rg    = grouped . sorted $ rr
        paths = map (namelessPath . L.head) rg
        ambig = paths \\ nub paths
    in if null ambig then rg else error (ambiguityMessage ambig)
  where
    sorted :: [Route a m] -> [Route a m]
    sorted = sortBy (compare `on` _path)

    grouped :: [Route a m] -> [[Route a m]]
    grouped = groupBy ((==) `on` _path)

    namelessPath :: Route a m -> ByteString
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
select :: Monad m => Renderer -> [Route a m] -> RoutingReq -> m Response
select render rr req = do
    let ms = filter ((method req ==) . _method) rr
    if null ms
        then return $ respond render (Error status405 Nothing) [(allow, validMethods)]
        else evalAll ms
  where
    allow :: HeaderName
    allow = mk "Allow"

    validMethods :: ByteString
    validMethods = C.intercalate "," $ nub (C.pack . show . _method <$> rr)

    evalAll :: Monad m => [Route a m] -> m Response
    evalAll rs =
        let (n, y) = partitionEithers $ foldl' evalSingle [] rs
        in if null y
            then return $ respond render (L.head n) []
            else closest y

    evalSingle :: Monad m => [Either Error (Handler m)] -> Route a m -> [Either Error (Handler m)]
    evalSingle rs r =
        case _pred r of
            Pack p h -> case p req of
                Fail   m -> Left m : rs
                Okay d v -> Right (Handler d (h v)) : rs

    closest :: Monad m => [Handler m] -> m Response
    closest hh = case map _handler . sortBy (compare `on` _delta) $ hh of
        []  -> return $ responseBuilder status404 [] mempty
        h:_ -> h

respond :: Renderer -> Error -> ResponseHeaders -> Response
respond f e h = responseLBS (status e) h (fromMaybe mempty (f e))

