-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Network.Wai.Predicate.Tutorial
  ( -- * Motivation
    -- $motivation

    -- * Introduction
    -- $introduction

    -- * Example Predicate
    -- $example

    -- * Routes
    -- $routes
  )
where

{- $motivation

The purpose of the @snap-predicates@ package is to facilitate the
convenient definition of safe Snap handlers. Here safety
means that a handler can declare all pre-conditions which must be
fulfilled such that the handler can produce a successful response.
It is then statically guaranteed that the handler will not be
invoked if any of these pre-conditions fails.

-}

{- $introduction

The @snap-predicates@ package defines a 'Data.Predicate.Boolean' type
which carries \-\- in addition to actual truth values 'Data.Predicate.T'
and 'Data.Predicate.F' \-\- meta-data for each case:

@
data 'Data.Predicate.Boolean' f t =
    'Data.Predicate.F' (Maybe f)
  | 'Data.Predicate.T' 'Data.Predicate.Delta' t
  deriving (Eq, Show)
@

'Data.Predicate.Delta' can in most instances be ignored, i.e. set to 0.
It's purpose is as a measure of distance for those predicates which evaluate
to 'Data.Predicate.T' but some may be \"closer\" in some way than others. An
example is for instance HTTP content-negotiations (cf. 'Snap.Predicate.Accept.Accept')

Further there is a type-class 'Data.Predicate.Predicate' defined which
contains an evaluation function 'Data.Predicate.apply', where the
predicate instance is applied to some value, yielding 'Data.Predicate.T'
or 'Data.Predicate.F'.

@
class 'Data.Predicate.Predicate' p a where
    type 'Data.Predicate.FVal' p
    type 'Data.Predicate.TVal' p
    apply :: p -> a -> Boolean ('Data.Predicate.FVal' p) ('Data.Predicate.TVal' p)
@

All predicates are instances of this type-class, which does not
specify the type against which the predicate is evaluated, nor the types
of actual meta-data for the true/false case of the Boolean returned.
Snap related predicates are normally defined against 'Snap.Core.Request'
and in case they fail, they return a status code and an optional message.

Besides these type definitions, there are some ways to connect two
'Data.Predicate.Predicate's to form a new one as the logical @OR@ or the
logical @AND@ of its parts. These are:

  * 'Data.Predicate.:|:' and 'Data.Predicate.:||:' as logical @OR@s

  * 'Data.Predicate.:&:' as logical @AND@

In addition to evaluating to 'Data.Predicate.T' or 'Data.Predicate.F'
depending on the truth values of its parts, these connectives also
propagate the meta-data and 'Data.Predicate.Delta' appropriately.

If 'Data.Predicate.:&:' evaluates to 'Data.Predicate.T' it has to combine
the meta-data of both predicates, and it uses the product type
'Data.Predicate.:*:' for this. This type also has a right-associative
data constructor using the same symbol, so one can combine many predicates
without having to nest the meta-data pairs.

In the @OR@ case, the two predicates have potentially meta-data of
different types, so we use a sum type 'Either' whenever we combine
two predicates with 'Data.Predicate.:||:'. For convenience a type-alias
'Data.Predicate.:+:' is defined for 'Either', which allows simple infix
notation. However, for the common case where both predicates have
meta-data of the same type, there is often no need to distinguish which
@OR@-branch was true. In this case, the 'Data.Predicate.:|:' combinator
can be used.

Finally there are 'Data.Predicate.Const' and 'Data.Predicate.Fail' to
always evaluate to 'Data.Predicate.T' or 'Data.Predicate.F' respectively.

As an example of how these operators are used, see below in section \"Routes\".
-}

{- $example

@
data Param = Param 'Data.ByteString.ByteString' deriving Eq

instance 'Data.Predicate.Predicate' Param 'Snap.Core.Request' where
    type 'Data.Predicate.FVal' Param = 'Snap.Predicate.Error'
    type 'Data.Predicate.TVal' Param = ByteString
    apply (Param x) r =
        case params r x of
            []    -> F ('Snap.Predicate.Error' 400 (Just $ \"Expected parameter '\" \<\> x \<\> \"'.\"))
            (v:_) -> T [] v
@

This is a simple example looking for the existence of a 'Snap.Core.Request' parameter
with the given name. In the success case, the parameter value is returned.

As mentioned before, Snap predicates usually fix the type @a@ from
'Data.Predicate.Predicate' above to 'Snap.Core.Request'. The associated
types 'Data.Predicate.FVal' and 'Data.Predicate.TVal' denote the meta-data
types of the predicate. In this example, the meta-date type is 'Data.ByteString.ByteString'.
The 'Data.Predicate.F'-case is 'Snap.Predicate.Error' which contains a status
code and an optional message.

-}

{- $routes

So how are 'Data.Predicate.Predicate's used in some Snap application?
One way is to just evaluate them against a given request inside a snap handler, e.g.

@
someHandler :: 'Snap.Core.Snap' ()
someHandler = do
    req <- 'Snap.Core.getRequest'
    case 'Data.Predicate.apply ('Snap.Predicate.Accept.accept' 'Data.Predicate.:&:' 'Snap.Predicate.Param.param' \"baz\") req of
        'Data.Predicate.T' ((_ :: 'Snap.Predicate.MediaType.Media' \"text\" \"plain\") 'Data.Predicate.:*:' bazValue) -> ...
        'Data.Predicate.F' (Just ('Snap.Predicate.Error' st msg))                      -> ...
        'Data.Predicate.F' Nothing                                    -> ...
@

However another possibility is to augment route definitions with the
'Snap.Routes.Routes' monad to use them with 'Snap.Core.route', e.g.

@
sitemap :: 'Snap.Routes.Routes' Snap ()
sitemap = do
    'Snap.Routes.get'  \"\/a\" handlerA $ 'Snap.Predicate.Accept.accept' 'Data.Predicate.:&:' ('Snap.Predicate.Param.param' \"name\" 'Data.Predicate.:|:' 'Snap.Predicate.Param.param' \"nick\") 'Data.Predicate.:&:' 'Snap.Predicate.Param.param' \"foo\"
    'Snap.Routes.get'  \"\/b\" handlerB $ 'Snap.Predicate.Accept.accept' 'Data.Predicate.:&:' ('Snap.Predicate.Param.param' \"name\" 'Data.Predicate.:||:' 'Snap.Predicate.Param.param' \"nick\") 'Data.Predicate.:&:' 'Snap.Predicate.Param.param' \"foo\"
    'Snap.Routes.get'  \"\/c\" handlerC $ 'Data.Predicate.Fail' ('Snap.Predicate.Error' 410 (Just \"Gone.\"))
    'Snap.Routes.post' \"\/d\" handlerD $ 'Snap.Predicate.Accept.accept'
    'Snap.Routes.post' \"\/e\" handlerE $ 'Snap.Predicate.Accept.accept'
@

The handlers then encode their pre-conditions in their type-signature:

@
handlerA :: 'Snap.Predicate.MediaType.Media' \"application\" \"json\" 'Data.Predicate.:*:' ByteString 'Data.Predicate.:*:' ByteString -> Snap ()
handlerB :: 'Snap.Predicate.MediaType.Media' \"text\" \"plain\" 'Data.Predicate.:*:' (ByteString 'Data.Predicate.:+:' ByteString) 'Data.Predicate.:*:' ByteString -> Snap ()
handlerC :: 'Snap.Predicate.MediaType.Media' \"application\" \"json\" 'Data.Predicate.:*:' Char -> Snap ()
handlerD :: 'Snap.Predicate.MediaType.Media' \"application\" \"x-protobuf\" -> Snap ()
handlerE :: 'Snap.Predicate.MediaType.Media' \"application\" \"xml\" -> Snap ()
@

The type-declaration of a handler has to match the corresponding predicate,
i.e. the type of the predicate's 'Data.Predicate.T' meta-data value:

@
('Snap.Core.MonadSnap' m, 'Data.Predicate.Predicate' p 'Snap.Core.Request') => 'Data.Predicate.TVal' p -> m ()
@

One thing to note is that 'Data.Predicate.Fail' works with
all 'Data.Predicate.T' meta-data types which is safe because the handler is never
invoked, or 'Data.Predicate.Fail' is used in some logical disjunction.

Given the route and handler definitions above, one can then integrate
with Snap via 'Snap.Routes.expandRoutes', which turns the
'Snap.Routes.Routes' monad into a list of
@'Snap.Core.MonadSnap' m => [(ByteString, m ())]@.
Additionally routes can be turned into Strings via 'Snap.Routes.showRoutes'.

-}
