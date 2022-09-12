# Changelog

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
