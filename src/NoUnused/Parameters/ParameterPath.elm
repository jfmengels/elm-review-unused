module NoUnused.Parameters.ParameterPath exposing
    ( Nesting
    , Path
    , canBeFixed
    , inAlias
    , inNamedPattern
    , inRecord
    , inTuple
    , init
    , nextArgument
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


nextArgument : Path -> Path
nextArgument path =
    { index = path.index + 1
    , nesting = Array.empty
    }


inRecord : Path -> Path
inRecord path =
    { index = path.index
    , nesting = Array.push RecordField path.nesting
    }


inTuple : Int -> Path -> Path
inTuple index path =
    { index = path.index
    , nesting = Array.push (TupleField index) path.nesting
    }


inNamedPattern : Int -> Path -> Path
inNamedPattern index path =
    { index = path.index
    , nesting = Array.push (NamedPattern index) path.nesting
    }


inAlias : Path -> Path
inAlias path =
    { index = path.index
    , nesting = Array.push AliasPattern path.nesting
    }


canBeFixed : Path -> Bool
canBeFixed path =
    Array.isEmpty path.nesting
