# review-unused

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to detect unused elements in your Elm project.

## Provided rules

- [`NoUnused.Variables`](https://package.elm-lang.org/packages/jfmengels/review-unused/latest/NoUnused-Variables) - Reports unused top-level variables and types, imports and imported variables and types inside of a module.
- [`NoUnused.CustomTypeConstructors`](https://package.elm-lang.org/packages/jfmengels/review-unused/latest/NoUnused-CustomTypeConstructors) - Reports unused constructors for a custom type.
- [`NoUnused.Exports`](https://package.elm-lang.org/packages/jfmengels/review-unused/latest/NoUnused-Exports) - Reports unused constructorsexposed elements from a module.

## Example configuration

```elm
module ReviewConfig exposing (config)

import NoUnused.CustomTypeConstructors
import NoUnused.Exports
import NoUnused.Variables
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.CustomTypeConstructors.rule
    , NoUnused.Exports.rule
    , NoUnused.Variables.rule
    ]
```
