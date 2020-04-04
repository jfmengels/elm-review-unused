# review-unused

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to detect unused elements in your Elm project.

## Provided rules

- [`NoUnused.Variables`](https://package.elm-lang.org/packages/jfmengels/review-unused/latest/NoUnused-Variables) - Reports unused top-level variables and types, imports and imported variables and types inside of a module.
- [`NoUnused.CustomTypeConstructors`](https://package.elm-lang.org/packages/jfmengels/review-unused/latest/NoUnused-CustomTypeConstructors) - Reports unused constructors for a custom type.
- [`NoUnused.Exports`](https://package.elm-lang.org/packages/jfmengels/review-unused/latest/NoUnused-Exports) - Reports unused exposed elements from a module.
- [`NoUnused.Modules`](https://package.elm-lang.org/packages/jfmengels/review-unused/latest/NoUnused-Modules) - Reports unused modules in the project.
- [`NoUnused.Dependencies`](https://package.elm-lang.org/packages/jfmengels/review-unused/latest/NoUnused-Dependencies) - Reports unused dependencies in the project.

## Example configuration

```elm
module ReviewConfig exposing (config)

import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Variables
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.CustomTypeConstructors.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Variables.rule
    ]
```


## How this package works

This package works by having several rules that check for different unused elements, and that complement each other.

This allows for fine-grained control over what you want the rules to do. If you add these rules to an existing project, you will likely get a lot of errors, and fixing them will take time. Instead, you can introduce these rules gradually in batches. For cases where the errors are too time-consuming to fix, you can ignore them in the configuration, until you take care of them.
