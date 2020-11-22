module NoUnused.RecordFields.Variable exposing (Register, Variable, declaredFields, markAsUsed, markAsUsedInAnUnknownManner, markFieldAsUsed, newVariable, unusedDeclaredFields, updateVariable, usedFields, wasUsed, wasUsedInAnUnknownManner)

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


type alias Register =
    Dict String Variable


updateVariable : String -> (Variable -> Variable) -> Register -> Register
updateVariable name function variables =
    Dict.update name (Maybe.map function) variables


unusedDeclaredFields : Register -> List (Node String)
unusedDeclaredFields register =
    register
        |> Dict.values
        |> List.concatMap unusedDeclaredFieldsForVariable


unusedDeclaredFieldsForVariable : Variable -> List (Node String)
unusedDeclaredFieldsForVariable (Variable variable) =
    if variable.wasUsedInAnUnknownManner || not variable.wasUsed then
        []

    else
        variable.declaredFields
            |> List.filter (\node -> not <| Set.member (Node.value node) variable.usedFields)
