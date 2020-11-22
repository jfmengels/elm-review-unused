module NoUnused.RecordFields.Variable exposing
    ( Register
    , Variable
    , addVariables
    , addVariablesInNewScope
    , declaredFields
    , emptyRegister
    , markAsUsed
    , markAsUsedInAnUnknownManner
    , markFieldAsUsed
    , markFieldsAsUsed
    , newVariable
    , unusedDeclaredFieldsForScope
    , updateVariable
    , usedFields
    , wasUsed
    , wasUsedInAnUnknownManner
    )

import Dict exposing (Dict)
import Elm.Syntax.Node as Node exposing (Node)
import Set exposing (Set)



-- VARIABLE


type Variable
    = Variable
        { usedFields : Set String
        , declaredFields : List (Node String)
        , wasUsed : Bool
        , wasUsedInAnUnknownManner : Bool
        }


newVariable : List (Node String) -> Set String -> Variable
newVariable declaredFields_ usedFields_ =
    Variable
        { usedFields = usedFields_
        , declaredFields = declaredFields_
        , wasUsed = False
        , wasUsedInAnUnknownManner = False
        }


markFieldAsUsed : String -> Variable -> Variable
markFieldAsUsed field (Variable variable) =
    Variable
        { variable
            | wasUsed = True
            , usedFields = Set.insert field variable.usedFields
        }


markFieldsAsUsed : List String -> Variable -> Variable
markFieldsAsUsed fields (Variable variable) =
    Variable
        { variable
            | wasUsed = True
            , usedFields = Set.union (Set.fromList fields) variable.usedFields
        }


markAsUsed : Variable -> Variable
markAsUsed (Variable variable) =
    Variable { variable | wasUsed = True }


markAsUsedInAnUnknownManner : Variable -> Variable
markAsUsedInAnUnknownManner (Variable variable) =
    Variable { variable | wasUsed = True, wasUsedInAnUnknownManner = True }


usedFields : Variable -> Set String
usedFields (Variable v) =
    v.usedFields


declaredFields : Variable -> List (Node String)
declaredFields (Variable v) =
    v.declaredFields


wasUsed : Variable -> Bool
wasUsed (Variable v) =
    v.wasUsed


wasUsedInAnUnknownManner : Variable -> Bool
wasUsedInAnUnknownManner (Variable v) =
    v.wasUsedInAnUnknownManner



-- REGISTER


type Register
    = Register (Dict String Variable) (List (Dict String Variable))


emptyRegister : Register
emptyRegister =
    Register Dict.empty []


addVariables : List ( String, Variable ) -> Register -> Register
addVariables list (Register register stack) =
    Register
        (List.foldl
            (\( name, variable ) dict -> Dict.insert name variable dict)
            register
            list
        )
        stack


addVariablesInNewScope : List ( String, Variable ) -> Register -> Register
addVariablesInNewScope list (Register register stack) =
    addVariables list (Register Dict.empty (register :: stack))


updateVariable : String -> (Variable -> Variable) -> Register -> Register
updateVariable name function (Register register stack) =
    let
        ( newRegister, newStack ) =
            updateVariableInternal name function register stack
    in
    Register newRegister newStack


updateVariableInternal : String -> (Variable -> Variable) -> Dict String Variable -> List (Dict String Variable) -> ( Dict String Variable, List (Dict String Variable) )
updateVariableInternal name function register stack =
    case Dict.get name register of
        Just variable ->
            ( Dict.insert name (function variable) register, stack )

        Nothing ->
            case stack of
                [] ->
                    ( register, stack )

                x :: xs ->
                    let
                        ( newRegister, newStack ) =
                            updateVariableInternal name function x xs
                    in
                    ( register, newRegister :: newStack )


unusedDeclaredFieldsForScope : Register -> ( List (Node String), Register )
unusedDeclaredFieldsForScope (Register register stack) =
    let
        unusedFields : List (Node String)
        unusedFields =
            register
                |> Dict.values
                |> List.concatMap unusedDeclaredFieldsForVariable

        newRegister : Register
        newRegister =
            case stack of
                head :: rest ->
                    Register head rest

                [] ->
                    Register Dict.empty []
    in
    ( unusedFields, newRegister )


unusedDeclaredFieldsForVariable : Variable -> List (Node String)
unusedDeclaredFieldsForVariable (Variable variable) =
    if variable.wasUsedInAnUnknownManner || not variable.wasUsed then
        []

    else
        variable.declaredFields
            |> List.filter (\node -> not <| Set.member (Node.value node) variable.usedFields)
