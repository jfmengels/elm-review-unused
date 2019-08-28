# lint-unused

[`elm-lint`](https://package.elm-lang.org/packages/jfmengels/elm-lint/latest)
plugin that provides rules to detect unused functions and types in your Elm files.

## Provided rules

- [`NoUnused.Variables`](./NoUnused-Variables) - Reports unused top-level variables and types, imports and imported variables and types.
- [`NoUnused.CustomTypeConstructors`](./NoUnused-CustomTypeConstructors) - Reports unused constructors for a custom type.

### Note

Since `elm-lint` only works in the scope of a single file, these rules
will not report elements that are exposed but not used anywhere in the project.
If you wish those to be reported, check out [`elm-xref`](https://github.com/zwilias/elm-xref).
