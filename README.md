wai-routing enables the declaration of "routes" which handle
requests to a specific URL.

The set of possible handlers can be restricted by "predicates",
which operate on WAI requests and have to be true or else the
handler will not be called.

For details have a look at the haddock documentation of
`Network.Wai.Routing.Tutorial` or the `examples` folder in the
source distribution.

This library is a port of `snap-predicates` which provides
similar functionality for the snap framework.

The routing tree construction is implemented using `wai-route`.

Here is a simple usage example.


```haskell
import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.Wai
import Network.Wai.Routing
import Network.Wai.Handler.Warp

main :: IO ()
main = run 8080 (route (prepare start))

start :: Monad m => Routes m a ()
start = do
    get "/user/:name" fetchUser $
        Capture "name"

    get "/user/find" findUser $
        Query "byName" :||: Query "byId"

    delete "/user/:name" rmUser $
        Capture "name" :&: Opt (Cookie "foo")

fetchUser :: Monad m => Text -> m Response
fetchUser name = ...

findUser :: Monad m => Either ByteString Word64 -> m Response
findUser (Left  name)  = ...
findUser (Right ident) = ...

rmUser :: Monad m => Text ::: Maybe Int -> m Response
rmUser (name ::: foo)  = ...
```

