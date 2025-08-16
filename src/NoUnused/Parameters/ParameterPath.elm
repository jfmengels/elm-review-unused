module NoUnused.Parameters.ParameterPath exposing
    ( Nesting(..)
    , Path
    , canBeFixed
    , init
    , push
    )

import Array exposing (Array)


type alias Path =
    { index : Int
    , nesting : Array Nesting
    }


type Nesting
    = RecordField
    | TupleField Int
    | NamedPattern Int
    | AliasPattern


init : Int -> Path
init index =
    { index = index
    , nesting = Array.empty
    }


push : Nesting -> Path -> Path
push pathInArgument path =
    { index = path.index
    , nesting = Array.push pathInArgument path.nesting
    }


canBeFixed : Path -> Bool
canBeFixed path =
    Array.isEmpty path.nesting
