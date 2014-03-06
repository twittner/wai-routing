wai-routing enables the declaration of "routes" which handle
requests to a specific URL.

The set of possible handlers can be restricted by "predicates",
which operate on WAI requests and have to be true or else the
handler will not be called.
