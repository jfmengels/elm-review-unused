# Changelog

## [Unreleased]

Fixed issues in [`NoUnused.Exports`] where type aliases and custom types would incorrectly be reported. Thanks Eric from the Elm Slack for reporting!

## [1.2.1] - 2024-04-01

- [`NoUnused.Exports`] now reports (and removes using `--fix`) elements even if the module is exposing everything (`module X exposing (..)`) if the element is unused both locally and across the project. Thanks [@tfausak](https://github.com/tfausak).

## [1.2.0] - 2023-07-28

Add configuration setting for [`NoUnused.Exports`] for reporting exports that is only used in non-production code. This includes multiple new functions and types in that module centered around `reportUnusedProductionExports`. 

## [1.1.30] - 2023-06-16

Fix false positive in [`NoUnused.CustomTypeConstructors`] related to non-ASCII constructor names ([#79](https://github.com/jfmengels/elm-review-unused/pull/79)). Thanks [@lydell](https://github.com/lydell) and [@marc136](https://github.com/marc136).

## [1.1.29] - 2022-12-07

Fix false positive in [`NoUnused.CustomTypeConstructors`].

## [1.1.28] - 2022-11-08

Add better support for `jfmengels/elm-review` v2.10.0.

## [1.1.27] - 2022-09-19

This release contains HUGE performance updates. Rough benchmarks show some of the slow rules ([`NoUnused.Exports`], [`NoUnused.CustomTypeConstructors`] and [`NoUnused.CustomTypeConstructorArgs`]) are now 20-40 times faster compared to the previous release.

## [1.1.26] - 2022-09-11

- Fixed an issue in [`NoUnused.Dependencies`] that led the `elm.json` to become corrupt

## [1.1.25] - 2022-09-10

- [`NoUnused.Patterns`] now reports multiple aliases `(((A a) as y) as z)`
- [`NoUnused.Patterns`] now reports aliases on a variable pattern `(name as otherName)`
- [`NoUnused.Patterns`] now reports aliases to wildcard `(_ as thing)` in parameters, and [`NoUnused.Parameters`] now doesn't
- Improved the fixes for [`NoUnused.Patterns`] to not include unnecessary patterns
- Improved the fixes for [`NoUnused.Patterns`] to have formatting look more like `elm-format`

## [1.1.24] - 2022-09-02

- [`NoUnused.Variables`] now reports imports that get shadowed by other imports ([252475888b79a88f107571c1002d0ed650622ddb])

## [1.1.23] - 2022-08-24

This version merges the [`NoUnused.Modules`] into the [`NoUnused.Exports`] rule.

A common issue when running `elm-review --fix` (or `--fix-all`) was when you had the two rules enabled and encountered an unused module.

While in fix mode, `NoUnused.Exports` would remove every export one at a time, which would likely be followed by
[`NoUnused.Variables`] removing the previously exported element. This would go on until the module is as empty as it can
be. At this point, you would finally be able to see `NoUnused.Modules`'s error indicating that the module is unused.

Whether you want to remove the module or use it somewhere in response to this message, this is a lot of unnecessary work
for you and/or the tool, making `--fix-all` painfully long.

By having the `NoUnused.Exports` do the work of both rules, and not reporting any unused exports when the entire module
is unused, this situation should not happen anymore, or not as exacerbated.

[`NoUnused.Modules`] is therefore now deprecated and should not be used anymore. It is removed from the `example`
configuration.


## [1.1.22] - 2022-04-14

- Fixed an issue in [`NoUnused.Variables`] where removing a let declaration would not always remove its type annotation ([24237116ada98791d8ff79630ca5d4eb632ef6ea])


## Missing changelog

Help would be appreciated to fill the blanks!

[Unreleased]: https://github.com/jfmengels/elm-review-unused/compare/v1.2.1...HEAD
[1.2.1]: https://github.com/jfmengels/elm-review-unused/releases/tag/1.2.1
[1.2.0]: https://github.com/jfmengels/elm-review-unused/releases/tag/1.2.0
[1.1.30]: https://github.com/jfmengels/elm-review-unused/releases/tag/1.1.30
[1.1.29]: https://github.com/jfmengels/elm-review-unused/releases/tag/1.1.29
[1.1.28]: https://github.com/jfmengels/elm-review-unused/releases/tag/1.1.28
[1.1.27]: https://github.com/jfmengels/elm-review-unused/releases/tag/1.1.27
[1.1.26]: https://github.com/jfmengels/elm-review-unused/releases/tag/1.1.26
[1.1.25]: https://github.com/jfmengels/elm-review-unused/releases/tag/1.1.25
[1.1.24]: https://github.com/jfmengels/elm-review-unused/releases/tag/1.1.24
[1.1.23]: https://github.com/jfmengels/elm-review-unused/releases/tag/1.1.23
[1.1.22]: https://github.com/jfmengels/elm-review-unused/releases/tag/1.1.22

[252475888b79a88f107571c1002d0ed650622ddb]: https://github.com/jfmengels/elm-review-unused/commit/252475888b79a88f107571c1002d0ed650622ddb
[24237116ada98791d8ff79630ca5d4eb632ef6ea]: https://github.com/jfmengels/elm-review-unused/commit/24237116ada98791d8ff79630ca5d4eb632ef6ea

[`NoUnused.Modules`]: (https://package.elm-lang.org/packages/jfmengels/elm-review-unused/latest/NoUnused-Modules)
[`NoUnused.Exports`]: (https://package.elm-lang.org/packages/jfmengels/elm-review-unused/latest/NoUnused-Exports)
[`NoUnused.Variables`]: (https://package.elm-lang.org/packages/jfmengels/elm-review-unused/latest/NoUnused-Variables)
[`NoUnused.Patterns`]: (https://package.elm-lang.org/packages/jfmengels/elm-review-unused/latest/NoUnused-Patterns)
[`NoUnused.Parameters`]: (https://package.elm-lang.org/packages/jfmengels/elm-review-unused/latest/NoUnused-Parameters)
[`NoUnused.Dependencies`]: (https://package.elm-lang.org/packages/jfmengels/elm-review-unused/latest/NoUnused-Dependencies)
[`NoUnused.CustomTypeConstructors`]: (https://package.elm-lang.org/packages/jfmengels/elm-review-unused/latest/NoUnused-CustomTypeConstructors)
[`NoUnused.CustomTypeConstructorArgs`]: (https://package.elm-lang.org/packages/jfmengels/elm-review-unused/latest/NoUnused-CustomTypeConstructorArgs)
