-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Network.Wai.Routing.Tutorial
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

The purpose of the @wai-predicates@ package is to facilitate the
convenient definition of safe WAI 'Application's. Here safety
means that a handler can declare all pre-conditions which must be
fulfilled such that the handler can produce a successful response.
It is then statically guaranteed that the handler will not be
invoked if any of these pre-conditions fails.

-}

{- $introduction

The @wai-predicates@ package defines a 'Network.Wai.Routing.Predicate.Boolean' type
which carries \-\- in addition to actual truth values 'Network.Wai.Routing.Predicate.T'
and 'Network.Wai.Routing.Predicate.F' \-\- meta-data for each case:

@
data Boolean f t
    = F (Maybe f)
    | T Delta t
    deriving (Eq, Show)
@

'Network.Wai.Routing.Predicate.Predicate.Delta' can in most instances be ignored, i.e. set to 0.
It's purpose is as a measure of distance for those predicates which evaluate
to @T@ but some may be \"closer\" in some way than others. An
example is for instance HTTP content-negotiations (cf.
'Network.Wai.Routing.Predicate.Predicate.Accept.Accept')

Further there is a type-class 'Network.Wai.Routing.Predicate.Predicate.Predicate' defined which
contains an evaluation function 'Network.Wai.Routing.Predicate.Predicate.apply', where the
predicate instance is applied to some value, yielding @T@ or @F@.

@
class Predicate p a where
    type FVal p
    type TVal p
    apply :: p -> a -> Boolean (FVal p) (TVal p)
@

All predicates are instances of this type-class, which does not
specify the type against which the predicate is evaluated, nor the types
of actual meta-data for the true/false case of the Boolean returned.
WAI related predicates are defined against 'Network.Wai.Routing.Request'
which holds a regular 'Network.Wai.Request' and path capture variables.
In case predicates fail, they return a status code and an optional message.

Besides these type definitions, there are some ways to connect two
predicates to form a new one as the logical @OR@ or the
logical @AND@ of its parts. These are:

  * 'Network.Wai.Routing.Predicate.Predicate.:|:' and 'Network.Wai.Routing.Predicate.Predicate.:||:' as logical @OR@s

  * 'Network.Wai.Routing.Predicate.Predicate.:&:' as logical @AND@

In addition to evaluating to @T@ or @F@ depending on the truth values of
its parts, these connectives also propagate the meta-data and @Delta@
appropriately.

If @:&:@ evaluates to @F@ it has to combine the meta-data of both predicates,
and it uses the product type 'Network.Wai.Routing.Predicate.Predicate.:::' for this.
This type also has a data constructor with the same symbol, so one can
combine many predicates without having to nest the meta-data pairs.

In the @OR@ case, the two predicates have potentially meta-data of
different types, so we use a sum type 'Either' whenever we combine
two predicates with @:||:@. For convenience a type-alias
@:+:@ is defined for 'Either', which allows simple infix
notation. However, for the common case where both predicates have
meta-data of the same type, there is often no need to distinguish which
@OR@-branch was true. In this case, the @:|:@ combinator can be used.

Finally there are 'Network.Wai.Routing.Predicate.Predicate.Const' and
'Network.Wai.Routing.Predicate.Predicate.Fail' to always evaluate to @T@ or @F@
respectively.

As an example of how these operators are used, see below in section \"Routes\".
-}

{- $example

@
newtype Query = Query ByteString

instance Predicate Query Request where
    type FVal Query = Error
    type TVal Query = ByteString
    apply (Query x) r =
        case lookupQuery x r of
            []    -> F (Error 400 (Just $ \"Expected parameter '\" \<\> x \<\> \"'.\"))
            (v:_) -> T [] v
@

This is a simple example looking for the existence of a 'Request' query
parameter with the given name. In the success case, the query value is
returned.

As mentioned before, WAI predicates usually fix the type @a@ from
@Predicate@ above to 'Network.Wai.Routing.Request'. The associated
types 'Network.Wai.Routing.Predicate.Predicate.FVal' and
'Network.Wai.Routing.Predicate.Predicate.TVal' denote the meta-data
types of the predicate. In this example, the meta-date type is
'Data.ByteString.ByteString'. The @F@-case is 'Network.Wai.Routing.Error'
which contains a status code and an optional message.

-}

{- $routes

So how are @Predicate@s used in an application?
One way is to just evaluate them against a given request, e.g.

@
someHandler :: Application
someHandler r =
    case apply (Accept :&: Query \"baz\") r of
        T ((_ :: Media \"text\" \"plain\") ::: bazValue) -> ...
        F (Just (Error st msg))                      -> ...
        F Nothing                                    -> ...
@

However another possibility is to declare route definitions with the
'Network.Wai.Route' which then routes requests to handler using the library
'wai-routes'.

@
sitemap :: Routes ()
sitemap = do
    get  \"\/a\" handlerA $ Accept :&: (Query \"name\" :|: Query \"nick\") :&: Query \"foo\"
    get  \"\/b\" handlerB $ Accept :&: (Query \"name\" :||: Query \"nick\") :&: Query \"foo\"
    get  \"\/c\" handlerC $ Fail (Error 410 (Just \"Gone.\"))
    post \"\/d\" handlerD $ Accept
    post \"\/e\" handlerE $ Accept
@

The handlers then encode their pre-conditions in their type-signature:

@
handlerA :: Media \"application\" \"json\" ::: ByteString ::: ByteString -> IO Response
handlerB :: Media \"text\" \"plain\" ::: (ByteString :+: ByteString) ::: ByteString -> IO Response
handlerC :: Media \"application\" \"json\" ::: Char -> IO Response
handlerD :: Media \"application\" \"x-protobuf\" -> IO Response
handlerE :: Media \"application\" \"xml\" -> IO Response
@

The type-declaration of a handler has to match the corresponding predicate,
i.e. the type of the predicate's @T@ meta-data value.

One thing to note is that @Fail@ works with
all @T@ meta-data types which is safe because the handler is never
invoked, or @Fail@ is used in some logical disjunction.
-}

