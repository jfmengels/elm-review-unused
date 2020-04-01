# review-unused

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to detect unused elements in your Elm code.

## Provided rules

- [`NoUnused.Variables`](./NoUnused-Variables) - Reports unused top-level variables and types, imports and imported variables and types inside of a module.
- [`NoUnused.CustomTypeConstructors`](./NoUnused-CustomTypeConstructors) - Reports unused constructors for a custom type.

### Note

Since `elm-review` only works in the scope of a single file, these rules
will not report elements that are exposed but not used anywhere in the project.
If you wish those to be reported, check out [`elm-xref`](https://github.com/zwilias/elm-xref).

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
