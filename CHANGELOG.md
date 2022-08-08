# Changelog

## Unreleased

- Merge [`NoUnused.Modules`] into [`NoUnused.Exports`]

[`NoUnused.Exports`] will now report when a module is considered unused, and will in that case not report any errors about unused exposed elements.

[`NoUnused.Modules`] is now deprecated and you should stop using it. It will be removed at a later date.


## [1.1.22] - 2022-04-14

- Fixed an issue in [`NoUnused.Variables`] where removing a let declaration would not always remove its type annotation ([24237116ada98791d8ff79630ca5d4eb632ef6ea])


## Missing changelog

Help would be appreciated to fill the blanks!

[1.1.22]: https://github.com/jfmengels/elm-review-unused/releases/tag/1.1.22

[24237116ada98791d8ff79630ca5d4eb632ef6ea]: https://github.com/jfmengels/elm-review-unused/commit/24237116ada98791d8ff79630ca5d4eb632ef6ea


[`NoUnused.Modules`]: (https://package.elm-lang.org/packages/jfmengels/elm-review-unused/latest/NoUnused-Modules)
[`NoUnused.Exports`]: (https://package.elm-lang.org/packages/jfmengels/elm-review-unused/latest/NoUnused-Exports)
[`NoUnused.Variables`]: (https://package.elm-lang.org/packages/jfmengels/elm-review-unused/latest/NoUnused-Variables)