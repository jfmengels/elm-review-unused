module NoUnused.Parameters.ParameterPath exposing (PathInArgument(..))


type PathInArgument
    = RecordField
    | TupleField Int
    | NamedPattern Int
    | AliasPattern
