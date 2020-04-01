# review-unused

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to detect unused elements in your Elm code.

## Provided rules

- [`NoUnused.Variables`](./NoUnused-Variables) - Reports unused top-level variables and types, imports and imported variables and types inside of a module.
- [`NoUnused.CustomTypeConstructors`](./NoUnused-CustomTypeConstructors) - Reports unused constructors for a custom type.


## Example configuration

```elm
module ReviewConfig exposing (config)

import NoUnused.CustomTypeConstructors
import NoUnused.Variables
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.Variables.rule
    , NoUnused.CustomTypeConstructors.rule
    ]
```
