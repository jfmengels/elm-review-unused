# elm-review-unused

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to detect unused elements in your Elm project.

## Provided rules

- [🔧 `NoUnused.Variables`](https://package.elm-lang.org/packages/jfmengels/elm-review-unused/1.1.11/NoUnused-Variables "Provides automatic fixes") - Reports unused top-level variables and types, imports and imported variables and types inside of a module.
- [🔧 `NoUnused.CustomTypeConstructors`](https://package.elm-lang.org/packages/jfmengels/elm-review-unused/1.1.11/NoUnused-CustomTypeConstructors "Provides automatic fixes") - Reports unused constructors for a custom type.
- [`NoUnused.CustomTypeConstructorArgs`](https://package.elm-lang.org/packages/jfmengels/elm-review-unused/1.1.11/NoUnused-CustomTypeConstructorArgs "Provides automatic fixes") - Reports arguments of custom type constructors that are never used.
- [🔧 `NoUnused.Exports`](https://package.elm-lang.org/packages/jfmengels/elm-review-unused/1.1.11/NoUnused-Exports "Provides automatic fixes") - Reports unused exposed elements from a module.
- [`NoUnused.Modules`](https://package.elm-lang.org/packages/jfmengels/elm-review-unused/1.1.11/NoUnused-Modules "Provides automatic fixes") - Reports unused modules in the project.
- [🔧 `NoUnused.Dependencies`](https://package.elm-lang.org/packages/jfmengels/elm-review-unused/1.1.11/NoUnused-Dependencies "Provides automatic fixes") - Reports unused dependencies in the project.
- [🔧 `NoUnused.Parameters`](https://package.elm-lang.org/packages/jfmengels/elm-review-unused/1.1.11/NoUnused-Dependencies "Provides automatic fixes") - Report unused parameters.
- [🔧 `NoUnused.Patterns`](https://package.elm-lang.org/packages/jfmengels/elm-review-unused/1.1.11/NoUnused-Dependencies "Provides automatic fixes") - Report useless patterns and pattern values that are not used.

## Example configuration

```elm
module ReviewConfig exposing (config)

import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    ]
```


## How this package works

This package works by having several rules that check for different unused elements, and that complement each other.

This allows for fine-grained control over what you want the rules to do. If you add these rules to an existing project, you will likely get a lot of errors, and fixing them will take time (though the autofixing will definitely help). Instead, you can introduce these rules gradually in batches. For cases where the errors are too time-consuming to fix, you can ignore them in the configuration, until you take care of them.

A few of these rules provide automatic fixes using `elm-review --fix` or `elm-review --fix-all`.


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template jfmengels/elm-review-unused/example
```


## Thanks

Thanks to @sparksp for writing [`NoUnused.Parameters`](https://package.elm-lang.org/packages/jfmengels/elm-review-unused/1.1.11/NoUnused-Dependencies) and [`NoUnused.Patterns`](https://package.elm-lang.org/packages/jfmengels/elm-review-unused/1.0.1/NoUnused-Dependencies).
