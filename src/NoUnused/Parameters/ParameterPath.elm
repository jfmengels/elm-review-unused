module NoUnused.Parameters.ParameterPath exposing
    ( Nesting
    , Path
    , fix
    , fromSignature
    , inAlias
    , inNamedPattern
    , inRecord
    , inTuple
    , nextArgument
    )

import Array exposing (Array)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)


type alias Path =
    { index : Int
    , nesting : Array Nesting
    , functionSignature : FunctionSignature
    , typeSignature : TypeSignature
    }


type Nesting
    = RecordField
    | TupleField Int
    | NamedPattern Int
    | AliasPattern


type FunctionSignature
    = NoFunctionSignature
    | NoMoreFunctionArguments
    | FunctionSignature (Node TypeAnnotation)


type TypeSignature
    = Absent
    | NoCorrespondingArg
    | Present { removeFullArgRange : Range, typeAnnotation : Node TypeAnnotation }


fromSignature : Maybe (Node Signature) -> Path
fromSignature signature =
    let
        ( typeSignature, functionSignature ) =
            nextArgumentInTypeSignature
                (case signature of
                    Nothing ->
                        NoFunctionSignature

                    Just (Node _ signature_) ->
                        FunctionSignature signature_.typeAnnotation
                )
    in
    { index = 0
    , nesting = Array.empty
    , functionSignature = functionSignature
    , typeSignature = typeSignature
    }


nextArgumentInTypeSignature : FunctionSignature -> ( TypeSignature, FunctionSignature )
nextArgumentInTypeSignature functionSignature =
    case functionSignature of
        NoFunctionSignature ->
            ( Absent, NoFunctionSignature )

        NoMoreFunctionArguments ->
            ( NoCorrespondingArg, NoMoreFunctionArguments )

        FunctionSignature typeAnnotation ->
            case Node.value typeAnnotation of
                TypeAnnotation.FunctionTypeAnnotation typeAnnotation_ ((Node rangeOfNext _) as restOfTypeAnnotation_) ->
                    ( Present
                        { removeFullArgRange = { start = (Node.range typeAnnotation_).start, end = rangeOfNext.start }
                        , typeAnnotation = typeAnnotation_
                        }
                    , FunctionSignature restOfTypeAnnotation_
                    )

                _ ->
                    ( NoCorrespondingArg, NoMoreFunctionArguments )


nextArgument : Path -> Path
nextArgument path =
    let
        ( typeSignature, functionSignature ) =
            nextArgumentInTypeSignature path.functionSignature
    in
    { index = path.index + 1
    , nesting = Array.empty
    , functionSignature = functionSignature
    , typeSignature = typeSignature
    }


inRecord : Path -> Path
inRecord path =
    { index = path.index
    , nesting = Array.push RecordField path.nesting
    , functionSignature = path.functionSignature
    , typeSignature = path.typeSignature
    }


inTuple : Int -> Path -> Path
inTuple index path =
    { index = path.index
    , nesting = Array.push (TupleField index) path.nesting
    , functionSignature = path.functionSignature
    , typeSignature = path.typeSignature
    }


inNamedPattern : Int -> Path -> Path
inNamedPattern index path =
    { index = path.index
    , nesting = Array.push (NamedPattern index) path.nesting
    , functionSignature = path.functionSignature
    , typeSignature = path.typeSignature
    }


inAlias : Path -> Path
inAlias path =
    { index = path.index
    , nesting = Array.push AliasPattern path.nesting
    , functionSignature = path.functionSignature
    , typeSignature = path.typeSignature
    }


fix : Path -> List Range -> Maybe (List Range)
fix path edits =
    case path.typeSignature of
        Absent ->
            Just edits

        NoCorrespondingArg ->
            Nothing

        Present record ->
            if Array.isEmpty path.nesting then
                Just (record.removeFullArgRange :: edits)

            else
                Nothing
