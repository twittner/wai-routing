-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Routing.Route
    ( Routes
    , App
    , Continue
    , Meta (..)
    , prepare
    , route
    , continue
    , addRoute
    , attach
    , examine
    , get
    , Network.Wai.Routing.Route.head
    , post
    , put
    , delete
    , trace
    , options
    , connect
    , patch
    , Renderer
    , renderer
    ) where

import Control.Applicative hiding (Const)
import Control.Monad
import Control.Monad.Trans.State.Strict hiding (get, put)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.Either
import Data.Function
import Data.List hiding (head, delete)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Monoid
import Network.HTTP.Types
import Network.Wai
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
    , _handler :: m ResponseReceived
    }

data Pack m where
    Pack :: Predicate RoutingReq Error a
         -> (a -> Continue m -> m ResponseReceived)
         -> Pack m

-- | The WAI 3.0 application continuation for arbitrary @m@ instead of @IO@.
type Continue m = Response -> m ResponseReceived

-- | Similar to a WAI 'Application' but for 'RoutingReq' and not specific
-- to @IO@.
type App m = RoutingReq -> Continue m -> m ResponseReceived

-- | Function to turn an 'Error' value into a 'Lazy.ByteString'
-- to send as the response body and a list of additional response headers.
-- Clients can provide their own renderer using 'renderer'.
type Renderer = Error -> Maybe (Lazy.ByteString, [Header])

-- | Data added to a route via 'attach' is returned in this @Meta@ record.
data Meta a = Meta
    { routeMethod :: !Method
    , routePath   :: !ByteString
    , routeMeta   :: a
    }

-- | Set a custom render function, i.e. a function to turn 'Error's into
-- 'Lazy.ByteString's.
renderer :: Renderer -> Routes a m ()
renderer f = Routes . modify $ \s -> s { renderfn = f }

defRenderer :: Renderer
defRenderer e =
    let r = reason2str  <$> reason e
        s = source2str  <$> source e
        m = message2str <$> message e
        l = labels2str . map Lazy.fromStrict $ labels e
        x = case catMaybes [s, r, l] of
               [] -> Nothing
               xs -> Just (Lazy.intercalate " " xs)
    in plainText <$> maybe x (\y -> (<> (" -- " <> y)) <$> x) m
  where
    reason2str  NotAvailable = "not-available"
    reason2str  TypeError    = "type-error"
    source2str  s  = "'" <> Lazy.fromStrict s <> "'"
    message2str s  = Lazy.fromStrict s
    labels2str  [] = Nothing
    labels2str  xs = Just $ "[" <> Lazy.intercalate "," xs <> "]"
    plainText    s = (s, [("Content-Type", "text/plain")])

-- | The Routes monad state type.
data St a m = St
    { routes   :: [Route a m]
    , renderfn :: Renderer
    }

-- | Initial state.
zero :: St a m
zero = St [] defRenderer

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
--
-- A route handler is like a WAI 'Application' but instead of 'Request'
-- the first parameter is the result-type of the associated 'Predicate'
-- evaluation. I.e. the handler is applied to the predicate's metadata
-- value iff the predicate is true.
addRoute :: Monad m
         => Method
         -> ByteString                              -- ^ path
         -> (a -> Continue m -> m ResponseReceived) -- ^ handler
         -> Predicate RoutingReq Error a            -- ^ 'Predicate'
         -> Routes b m ()
addRoute m r x p = Routes . modify $ \s ->
    s { routes = Route m r Nothing (Pack p x) : routes s }

-- | Specialisation of 'addRoute' for a specific HTTP 'Method'.
get, head, post, put, delete, trace, options, connect, patch ::
    Monad m
    => ByteString                              -- ^ path
    -> (a -> Continue m -> m ResponseReceived) -- ^ handler
    -> Predicate RoutingReq Error a            -- ^ 'Predicate'
    -> Routes b m ()
get     = addRoute (renderStdMethod GET)
head    = addRoute (renderStdMethod HEAD)
post    = addRoute (renderStdMethod POST)
put     = addRoute (renderStdMethod PUT)
delete  = addRoute (renderStdMethod DELETE)
trace   = addRoute (renderStdMethod TRACE)
options = addRoute (renderStdMethod OPTIONS)
connect = addRoute (renderStdMethod CONNECT)
patch   = addRoute (renderStdMethod PATCH)

-- | Add some metadata to the last route.
attach :: a -> Routes a m ()
attach a = Routes $ modify addToLast
  where
    addToLast s@(St []   _) = s
    addToLast (St (r:rr) f) = St (r { _meta = Just a } : rr) f

-- | Get back all attached metadata.
examine :: Routes a m b -> [Meta a]
examine (Routes r) = let St rr _ = execState r zero in
    mapMaybe (\x -> Meta (_method x) (_path x) <$> _meta x) rr

-- | Routes requests to handlers based on predicated route declarations.
-- Note that @route (prepare ...)@ behaves like a WAI 'Application' generalised to
-- arbitrary monads.
route :: Monad m => [(ByteString, App m)] -> Request -> Continue m -> m ResponseReceived
route rm rq k = do
    let tr = Tree.fromList rm
    case Tree.lookup tr (Tree.segments $ rawPathInfo rq) of
        Just (f, v) -> f (fromReq v (fromRequest rq)) k
        Nothing     -> k notFound
  where
    notFound = responseLBS status404 [] ""

-- | Prior to WAI 3.0 applications returned a plain 'Response'. @continue@
-- turns such a function into a corresponding CPS version. For example:
--
-- @
-- sitemap :: Monad m => Routes a m ()
-- sitemap = do
--     get "\/f\/:foo" (/continue/ f) $ capture "foo"
--     get "\/g\/:foo" g            $ capture "foo"
--
-- f :: Monad m => Int -> m Response
-- f x = ...
--
-- g :: Monad m => Int -> Continue m -> m ResponseReceived
-- g x k = k $ ...
-- @
continue :: Monad m => (a -> m Response) -> a -> Continue m -> m ResponseReceived
continue f a k = f a >>= k
{-# INLINE continue #-}

-- | Run the 'Routes' monad and return the handlers per path.
prepare :: Monad m => Routes a m b -> [(ByteString, App m)]
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
select :: forall a m. Monad m => Renderer -> [Route a m] -> App m
select render rr req k = do
    let ms = filter ((method req ==) . _method) rr
    if null ms
        then k $ respond render e405 [(allow, validMethods)]
        else evalAll ms
  where
    evalAll :: [Route a m] -> m ResponseReceived
    evalAll rs =
        let (n, y) = partitionEithers $ foldl' evalSingle [] rs
        in if null y
            then k $ respond render (L.head n) []
            else closest y

    evalSingle :: [Either Error (Handler m)] -> Route a m -> [Either Error (Handler m)]
    evalSingle rs r =
        case _pred r of
            Pack p h -> case p req of
                Fail   m -> Left m : rs
                Okay d v -> Right (Handler d (h v k)) : rs

    closest :: [Handler m] -> m ResponseReceived
    closest hh = case map _handler . sortBy (compare `on` _delta) $ hh of
        []  -> k $ responseBuilder status404 [] mempty
        h:_ -> h

    validMethods :: ByteString
    validMethods = C.intercalate "," $ nub (C.pack . show . _method <$> rr)

allow :: HeaderName
allow = mk "Allow"

respond :: Renderer -> Error -> ResponseHeaders -> Response
respond f e h = responseLBS (status e) hdrs bdy
  where
    (bdy, hdrs) = case f e of
        Just (b, h') -> (b, h ++ h')
        Nothing      -> (mempty, h)
