0.12.1
-----------------------------------------------------------------------------
- Update `bytestring-conversion` upper bound.

0.12
-----------------------------------------------------------------------------
- Update `wai-route` dependency and change `prepare` and `route` to work on
  `Tree (App m)` instead of `[(ByteString, App m)]`.

0.11.1
-----------------------------------------------------------------------------
- Bugfix release

0.11
-----------------------------------------------------------------------------
- Change error renderer to return text and optionally additional response
  headers (Roman Borschel).
- Update upper bounds.

0.10.2
-----------------------------------------------------------------------------
- Update `bytestring-conversion` to version 0.2

0.10
-----------------------------------------------------------------------------
- Replace `bytestring-from` with `bytestring-conversion` and update test
  dependencies.

0.9.1
-----------------------------------------------------------------------------
- Bugfix release (benchmarks)

0.9
-----------------------------------------------------------------------------
- Update to WAI 3.0.x and honour the new CPS definition of `Application`
  (cf. the documentation of `Network.Wai.Routing.Route.continue` for details
  regarding the differences between old and new style handler types).

0.8
-----------------------------------------------------------------------------
- Update to `wai-predicates 0.6` and change default error renderer.
- Update `bytestring-from` dependency.

0.7
-----------------------------------------------------------------------------
- Update dependencies constraints.

0.6
-----------------------------------------------------------------------------
- Add `Meta` type to return route metadata added via `attach`
- Support `patch` for route declarations

0.5.1
-----------------------------------------------------------------------------
- Add `HasVault` instance

0.5
-----------------------------------------------------------------------------
- Update to `wai-predicates` 0.3

0.4.1
-----------------------------------------------------------------------------
- Update WAI version bounds to include 2.1

0.4
-----------------------------------------------------------------------------
- Move predicates into `wai-predicates` library and depend on version 0.2

0.3.1 [bugfix release]
-----------------------------------------------------------------------------
- Fix typo.


0.3
-----------------------------------------------------------------------------
- Add `getRequest` predicate.

0.2
-----------------------------------------------------------------------------
- Add `attach` and `examine` to add arbitrary data to declared routes.
- Rename `expand` to `prepare`.
- Change `route` to work on the result of `prepare`.
- Hide predicate constructors and provide functions instead.

0.1
-----------------------------------------------------------------------------
- Initial release.
