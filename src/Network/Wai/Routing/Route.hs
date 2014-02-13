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
    , route
    , expandRoutes
    , showRoutes
    , renderErrorWith

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
import Network.Wai (Response, Application, responseLBS, responseBuilder)
import Network.Wai (rawPathInfo)
import Network.Wai.Routing.Predicate
import Network.Wai.Routing.Error
import Network.Wai.Routing.Request

import qualified Data.ByteString.Char8  as C
import qualified Data.ByteString.Lazy   as Lazy
import qualified Data.List              as L
import qualified Network.Wai.Route.Tree as Tree

data Route = Route
    { _method  :: !Method
    , _path    :: !ByteString
    , _pred    :: !Pack
    }

data Handler = Handler
    { _delta   :: !Delta
    , _handler :: IO Response
    }

data Pack where
    Pack :: (Show p, Predicate p Request, FVal p ~ Error)
         => p
         -> (TVal p -> IO Response)
         -> Pack

-- | Function to turn an 'Error' value into a 'Lazy.ByteString'.
-- Clients can provide their own renderer using 'renderErrorWith'.
type Renderer = Error -> Maybe Lazy.ByteString

-- | The Routes monad state type.
data St = St [Route] Renderer

-- | Initial state.
zero :: St
zero = St [] (fmap Lazy.fromStrict . message)

-- | The Routes monad is used to add routing declarations
-- via 'addRoute' or one of 'get', 'post', etc.
newtype Routes a = Routes { _unroutes :: State St a }

instance Functor Routes where
    fmap = liftM

instance Applicative Routes where
    pure  = return
    (<*>) = ap

instance Monad Routes where
    return  = Routes . return
    m >>= f = Routes $ _unroutes m >>= _unroutes . f

-- | Add a route for some 'Method' and path (potentially with variable
-- captures), and constrained the some 'Predicate'.
addRoute :: (Show p, Predicate p Request, FVal p ~ Error)
         => Method
         -> ByteString               -- ^ path
         -> (TVal p -> IO Response)  -- ^ handler
         -> p                        -- ^ 'Predicate'
         -> Routes ()
addRoute m r x p = Routes . modify $ \(St !rr !f) ->
    St (Route m r (Pack p x) : rr) f

-- | Specialisation of 'addRoute' for a specific HTTP 'Method'.
get, head, post, put, delete, trace, options, connect ::
    (Show p, Predicate p Request, FVal p ~ Error)
    => ByteString               -- ^ path
    -> (TVal p -> IO Response)  -- ^ handler
    -> p                        -- ^ 'Predicate'
    -> Routes ()
get     = addRoute (renderStdMethod GET)
head    = addRoute (renderStdMethod HEAD)
post    = addRoute (renderStdMethod POST)
put     = addRoute (renderStdMethod PUT)
delete  = addRoute (renderStdMethod DELETE)
trace   = addRoute (renderStdMethod TRACE)
options = addRoute (renderStdMethod OPTIONS)
connect = addRoute (renderStdMethod CONNECT)

renderErrorWith :: Renderer -> Routes ()
renderErrorWith f = Routes . modify $ \(St !rr _) -> St rr f

-- | Turn route definitions into a list of 'String's.
showRoutes :: Routes a -> [String]
showRoutes (Routes routes) =
    let St rr _ = execState routes zero
    in flip map (concat (normalise rr)) $ \x ->
        case _pred x of
            Pack p _ -> shows (_method x)
                      . (' ':)
                      . shows (_path x)
                      . (' ':)
                      . shows p $ ""

-- | A WAI 'Application' which routes requests to handlers based on
-- predicated route declarations.
route :: Routes a -> Application
route rm rq = do
    let tr = Tree.fromList $ expandRoutes rm
    case Tree.lookup tr (Tree.segments $ rawPathInfo rq) of
        Just (f, v) -> f (fromWaiRequest v rq)
        Nothing     -> return notFound
  where
    notFound = responseLBS status404 [] ""

expandRoutes :: Routes a -> [(ByteString, Request -> IO Response)]
expandRoutes (Routes routes) =
    let St rr f = execState routes zero in
    map (\g -> (_path (L.head g), select f g)) (normalise rr)

-- | Group routes by path.
normalise :: [Route] -> [[Route]]
normalise rr =
    let rg    = grouped . sorted $ rr
        paths = map (namelessPath . L.head) rg
        ambig = paths \\ nub paths
    in if null ambig then rg else error (ambiguityMessage ambig)
  where
    sorted :: [Route] -> [Route]
    sorted = sortBy (compare `on` _path)

    grouped :: [Route] -> [[Route]]
    grouped = groupBy ((==) `on` _path)

    namelessPath :: Route -> ByteString
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
select :: Renderer -> [Route] -> Request -> IO Response
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

    evalAll :: [Route] -> IO Response
    evalAll rs =
        let (n, y) = partitionEithers $ foldl' evalSingle [] rs
        in if null y
            then return $ respond render (L.head n) []
            else closest y

    evalSingle :: [Either Error Handler] -> Route -> [Either Error Handler]
    evalSingle rs r =
        case _pred r of
            Pack p h -> case apply p req of
                F   m -> Left m : rs
                T d v -> Right (Handler d (h v)) : rs

    closest :: [Handler] -> IO Response
    closest hh = case map _handler . sortBy (compare `on` _delta) $ hh of
        []  -> return $ responseBuilder status404 [] mempty
        h:_ -> h

respond :: Renderer -> Error -> ResponseHeaders -> Response
respond f e h = responseLBS (status e) h (fromMaybe mempty (f e))