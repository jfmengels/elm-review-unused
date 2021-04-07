module NoUnused.CustomTypeConstructors exposing (rule)

{-| Forbid having unused custom type constructors inside the project.


# Rule

@docs rule

-}

import Dict exposing (Dict)
import Elm.Module
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type as Type
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import NoUnused.RangeDict as RangeDict exposing (RangeDict)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Forbid having unused custom type constructors.

    config =
        [ NoUnused.CustomTypeConstructors.rule []
        ]

Note that this rule reports any custom type constructor that isn't used
anywhere _in the project_.

If the project is a package and the module that declared the type is exposed and
the type's constructors are exposed, then the constructors will not be reported.

This does not prevent you from using phantom types: A constructor won't be reported if

  - It is the only constructor of a type that has no type variable
  - It has no parameters
  - It is used as an argument of a custom type, in the stead of a type variable that is not used in the definition in any of the type's constructors

**Note**: At the time of writing, there may be cases where phantom types are not well detected.
When an opaque type is defined in a dependency, we don't know whether a type variable should be considered as a phantom type.

Therefore, sometimes this rule will need some help, by having you tell it what type variables of which type is a phantom type variable.
That's what the argument to the rule is for.

To explain that the `a` in `type Id a = Id String` from the `IdModule` module
corresponds to a phantom type variable, you would configure the rule like this:

    config =
        [ NoUnused.CustomTypeConstructors.rule
            [ { moduleName = "IdModule"
              , typeName = "Id"
              , index = 0 -- Position of the phantom variable in the type's arguments
              }
            ]
        ]

This rule could do a much better job than it currently does at figuring this out,
by following the definitions of custom types and type aliases, until it finds out that the type
variable is not used, or that it hits the limit related to dependencies described above.
In the meantime, you can configure the rule with all the phantom type exceptions.

I would love help with improving this :)


## Fail

    module A exposing (a)

    type MyType
        = UsedType
        | UnusedType -- Will get reported

    a =
        UsedType


## Success

    module A exposing (ExposedType(..))

    type MyType
        = UsedType

    a =
        UsedType

    type ExposedType
        = A
        | B
        | C

    -----------------------
    module A exposing (..)

    type ExposedType
        = A
        | B
        | C


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-unused/example --rules NoUnused.CustomTypeConstructors
```

-}
rule : List { moduleName : String, typeName : String, index : Int } -> Rule
rule phantomTypes =
    Rule.newProjectRuleSchema "NoUnused.CustomTypeConstructors" (initialProjectContext phantomTypes)
        |> Rule.withContextFromImportedModules
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = Rule.initContextCreator fromProjectToModule |> Rule.withModuleNameLookupTable |> Rule.withMetadata
            , fromModuleToProject = Rule.initContextCreator fromModuleToProject |> Rule.withModuleKey |> Rule.withMetadata
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withFinalProjectEvaluation finalProjectEvaluation
        |> Rule.fromProjectRuleSchema



-- MODULE VISITOR


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor



-- CONTEXT


type alias ModuleNameAsString =
    String


type alias CustomTypeName =
    String


type alias ConstructorName =
    String


type ExposedConstructors
    = ExposedConstructors
        { moduleKey : Rule.ModuleKey
        , customTypes : Dict CustomTypeName (Dict ConstructorName ConstructorInformation)
        }


type alias ConstructorInformation =
    { name : String
    , rangeToReport : Range
    , rangeToRemove : Maybe Range
    }


type alias ProjectContext =
    { exposedModules : Set ModuleNameAsString
    , declaredConstructors : Dict ModuleNameAsString ExposedConstructors
    , usedConstructors : Dict ModuleNameAsString (Set ConstructorName)
    , phantomVariables : Dict ModuleName (List ( CustomTypeName, Int ))
    , wasUsedInLocationThatNeedsItself : Set ( ModuleNameAsString, ConstructorName )
    , wasUsedInComparisons : Set ( ModuleNameAsString, ConstructorName )
    , wasUsedInOtherModules : Set ( ModuleNameAsString, ConstructorName )
    , fixesForRemovingConstructor : Dict ( ModuleNameAsString, ConstructorName ) (List Fix)
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , exposedCustomTypesWithConstructors : Set CustomTypeName
    , isExposed : Bool
    , exposesEverything : Bool
    , exposedConstructors : Dict ModuleNameAsString ExposedConstructors
    , declaredTypesWithConstructors : Dict CustomTypeName (Dict ConstructorName ConstructorInformation)
    , usedFunctionsOrValues : Dict ModuleNameAsString (Set ConstructorName)
    , phantomVariables : Dict ModuleName (List ( CustomTypeName, Int ))
    , ignoreBlocks : List (RangeDict (Set ( ModuleName, String )))
    , constructorsToIgnore : List (Set ( ModuleName, String ))
    , wasUsedInLocationThatNeedsItself : Set ( ModuleNameAsString, ConstructorName )
    , wasUsedInComparisons : Set ( ModuleNameAsString, ConstructorName )
    , fixesForRemovingConstructor : Dict ( ModuleNameAsString, ConstructorName ) (List Fix)
    , ignoredComparisonRanges : List Range
    }


initialProjectContext : List { moduleName : String, typeName : String, index : Int } -> ProjectContext
initialProjectContext phantomTypes =
    { exposedModules = Set.empty
    , declaredConstructors = Dict.empty
    , usedConstructors = Dict.empty
    , phantomVariables =
        List.foldl
            (\{ moduleName, typeName, index } dict ->
                Dict.update (String.split "." moduleName)
                    (Maybe.withDefault [] >> (::) ( typeName, index ) >> Just)
                    dict
            )
            Dict.empty
            phantomTypes
    , wasUsedInLocationThatNeedsItself = Set.empty
    , wasUsedInComparisons = Set.empty
    , wasUsedInOtherModules = Set.empty
    , fixesForRemovingConstructor = Dict.empty
    }


fromProjectToModule : ModuleNameLookupTable -> Rule.Metadata -> ProjectContext -> ModuleContext
fromProjectToModule lookupTable metadata projectContext =
    { lookupTable = lookupTable
    , exposedCustomTypesWithConstructors = Set.empty
    , isExposed = Set.member (Rule.moduleNameFromMetadata metadata |> String.join ".") projectContext.exposedModules
    , exposedConstructors = projectContext.declaredConstructors
    , exposesEverything = False
    , declaredTypesWithConstructors = Dict.empty
    , usedFunctionsOrValues = Dict.empty
    , phantomVariables = projectContext.phantomVariables
    , ignoreBlocks = []
    , constructorsToIgnore = []
    , wasUsedInLocationThatNeedsItself = Set.empty
    , wasUsedInComparisons = Set.empty
    , fixesForRemovingConstructor = Dict.empty
    , ignoredComparisonRanges = []
    }


fromModuleToProject : Rule.ModuleKey -> Rule.Metadata -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey metadata moduleContext =
    let
        localUsed : Set ConstructorName
        localUsed =
            moduleContext.usedFunctionsOrValues
                |> Dict.get ""
                |> Maybe.withDefault Set.empty

        localPhantomTypes : List ( CustomTypeName, Int )
        localPhantomTypes =
            moduleContext.phantomVariables
                |> Dict.get []
                |> Maybe.withDefault []

        moduleName : ModuleName
        moduleName =
            Rule.moduleNameFromMetadata metadata

        moduleNameAsString : ModuleNameAsString
        moduleNameAsString =
            String.join "." moduleName
    in
    { exposedModules = Set.empty
    , declaredConstructors =
        if moduleContext.isExposed then
            if moduleContext.exposesEverything then
                Dict.empty

            else
                Dict.singleton
                    moduleNameAsString
                    (ExposedConstructors
                        { moduleKey = moduleKey
                        , customTypes =
                            moduleContext.declaredTypesWithConstructors
                                |> Dict.filter (\typeName _ -> not <| Set.member typeName moduleContext.exposedCustomTypesWithConstructors)
                        }
                    )

        else
            Dict.singleton
                moduleNameAsString
                (ExposedConstructors
                    { moduleKey = moduleKey
                    , customTypes = moduleContext.declaredTypesWithConstructors
                    }
                )
    , usedConstructors =
        moduleContext.usedFunctionsOrValues
            |> Dict.remove ""
            |> Dict.insert moduleNameAsString localUsed
    , phantomVariables = Dict.singleton moduleName localPhantomTypes
    , wasUsedInLocationThatNeedsItself =
        Set.map
            (\(( moduleName_, constructorName ) as untouched) ->
                if moduleName_ == "" then
                    ( moduleNameAsString, constructorName )

                else
                    untouched
            )
            moduleContext.wasUsedInLocationThatNeedsItself
    , wasUsedInComparisons =
        Set.map
            (\(( moduleName_, constructorName ) as untouched) ->
                if moduleName_ == "" then
                    ( moduleNameAsString, constructorName )

                else
                    untouched
            )
            moduleContext.wasUsedInComparisons
    , wasUsedInOtherModules =
        List.foldl
            (\( moduleName_, constructors ) acc ->
                Set.union
                    (Set.map (Tuple.pair moduleName_) constructors)
                    acc
            )
            Set.empty
            (Dict.toList <| Dict.remove "" moduleContext.usedFunctionsOrValues)
    , fixesForRemovingConstructor =
        mapDictKeys
            (\(( moduleName_, constructorName ) as untouched) ->
                if moduleName_ == "" then
                    ( moduleNameAsString, constructorName )

                else
                    untouched
            )
            moduleContext.fixesForRemovingConstructor
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { exposedModules = previousContext.exposedModules
    , declaredConstructors = Dict.union newContext.declaredConstructors previousContext.declaredConstructors
    , usedConstructors =
        Dict.merge
            Dict.insert
            (\key newUsed previousUsed dict -> Dict.insert key (Set.union newUsed previousUsed) dict)
            Dict.insert
            newContext.usedConstructors
            previousContext.usedConstructors
            Dict.empty
    , phantomVariables = Dict.union newContext.phantomVariables previousContext.phantomVariables
    , wasUsedInLocationThatNeedsItself = Set.union newContext.wasUsedInLocationThatNeedsItself previousContext.wasUsedInLocationThatNeedsItself
    , wasUsedInComparisons = Set.union newContext.wasUsedInComparisons previousContext.wasUsedInComparisons
    , wasUsedInOtherModules = Set.union newContext.wasUsedInOtherModules previousContext.wasUsedInOtherModules
    , fixesForRemovingConstructor =
        Dict.merge
            (\key a dict -> Dict.insert key a dict)
            (\key a b dict -> Dict.insert key (a ++ b) dict)
            (\key b dict -> Dict.insert key b dict)
            newContext.fixesForRemovingConstructor
            previousContext.fixesForRemovingConstructor
            Dict.empty
    }



-- ELM.JSON VISITOR


elmJsonVisitor : Maybe { elmJsonKey : Rule.ElmJsonKey, project : Elm.Project.Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeElmJson projectContext =
    case maybeElmJson |> Maybe.map .project of
        Just (Elm.Project.Package package) ->
            let
                exposedModules : List Elm.Module.Name
                exposedModules =
                    case package.exposed of
                        Elm.Project.ExposedList list ->
                            list

                        Elm.Project.ExposedDict list ->
                            List.concatMap Tuple.second list

                exposedNames : Set String
                exposedNames =
                    exposedModules
                        |> List.map Elm.Module.toString
                        |> Set.fromList
            in
            ( [], { projectContext | exposedModules = exposedNames } )

        Just (Elm.Project.Application _) ->
            ( [], projectContext )

        Nothing ->
            ( [], projectContext )



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List nothing, ModuleContext )
moduleDefinitionVisitor moduleNode context =
    case Module.exposingList (Node.value moduleNode) of
        Exposing.All _ ->
            ( [], { context | exposesEverything = True } )

        Exposing.Explicit list ->
            let
                names : List String
                names =
                    List.filterMap
                        (\node ->
                            case Node.value node of
                                Exposing.TypeExpose { name } ->
                                    Just name

                                _ ->
                                    Nothing
                        )
                        list
            in
            ( []
            , { context
                | exposedCustomTypesWithConstructors =
                    Set.union (Set.fromList names) context.exposedCustomTypesWithConstructors
              }
            )



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List nothing, ModuleContext )
declarationListVisitor nodes context =
    ( [], List.foldl register context nodes )


register : Node Declaration -> ModuleContext -> ModuleContext
register node context =
    case Node.value node of
        Declaration.CustomTypeDeclaration { name, generics, constructors } ->
            let
                nonPhantomVariables : Set String
                nonPhantomVariables =
                    constructors
                        |> List.concatMap (Node.value >> .arguments)
                        |> List.concatMap collectGenericsFromTypeAnnotation
                        |> Set.fromList

                phantomVariables : List ( String, Int )
                phantomVariables =
                    generics
                        |> List.map Node.value
                        |> List.indexedMap Tuple.pair
                        |> List.filter (\( _, genericName ) -> not <| Set.member genericName nonPhantomVariables)
                        |> List.map (\( indexOfPhantomVariable, _ ) -> ( Node.value name, indexOfPhantomVariable ))

                newPhantomVariables : Dict (List String) (List ( String, Int ))
                newPhantomVariables =
                    Dict.update
                        []
                        (\maybeSet ->
                            case maybeSet of
                                Just old ->
                                    Just (phantomVariables ++ old)

                                Nothing ->
                                    Just phantomVariables
                        )
                        context.phantomVariables
            in
            { context | phantomVariables = newPhantomVariables }

        _ ->
            context



-- DECLARATION VISITOR


declarationVisitor : Node Declaration -> ModuleContext -> ( List nothing, ModuleContext )
declarationVisitor node context =
    case Node.value node of
        Declaration.CustomTypeDeclaration { name, constructors } ->
            if isPhantomCustomType name constructors then
                ( [], context )

            else
                let
                    constructorsAndNext : List ( Maybe (Node Type.ValueConstructor), Node Type.ValueConstructor )
                    constructorsAndNext =
                        List.map2 Tuple.pair
                            (List.map Just (List.drop 1 constructors) ++ [ Nothing ])
                            constructors

                    constructorsForCustomType : Dict String ConstructorInformation
                    constructorsForCustomType =
                        List.foldl
                            (\( next, constructor ) ( prev, dict ) ->
                                let
                                    nameNode : Node String
                                    nameNode =
                                        (Node.value constructor).name

                                    constructorName : String
                                    constructorName =
                                        Node.value nameNode

                                    constructorInformation : ConstructorInformation
                                    constructorInformation =
                                        { name = constructorName
                                        , rangeToReport = Node.range nameNode
                                        , rangeToRemove = findRangeToRemove prev constructor next
                                        }
                                in
                                ( Just constructor
                                , Dict.insert
                                    constructorName
                                    constructorInformation
                                    dict
                                )
                            )
                            ( Nothing, Dict.empty )
                            constructorsAndNext
                            |> Tuple.second
                in
                ( []
                , { context
                    | declaredTypesWithConstructors =
                        Dict.insert
                            (Node.value name)
                            constructorsForCustomType
                            context.declaredTypesWithConstructors
                  }
                )

        Declaration.FunctionDeclaration function ->
            ( []
            , markPhantomTypesFromTypeAnnotationAsUsed
                (Maybe.map (Node.value >> .typeAnnotation) function.signature)
                { context | ignoredComparisonRanges = [] }
            )

        Declaration.AliasDeclaration { typeAnnotation } ->
            ( [], markPhantomTypesFromTypeAnnotationAsUsed (Just typeAnnotation) context )

        _ ->
            ( [], context )


findRangeToRemove : Maybe (Node a) -> Node Type.ValueConstructor -> Maybe (Node c) -> Maybe { start : Elm.Syntax.Range.Location, end : Elm.Syntax.Range.Location }
findRangeToRemove previousConstructor constructor nextConstructor =
    case previousConstructor of
        Just prev ->
            Just
                { start = (Node.range prev).end
                , end = (Node.range constructor).end
                }

        Nothing ->
            case nextConstructor of
                Just next ->
                    Just
                        { start = constructor |> Node.value |> .name |> Node.range |> .start
                        , end = (Node.range next).start
                        }

                Nothing ->
                    Nothing


isPhantomCustomType : Node String -> List (Node Type.ValueConstructor) -> Bool
isPhantomCustomType name constructors =
    case constructors of
        (Node _ constructor) :: [] ->
            if Node.value name == Node.value constructor.name then
                case constructor.arguments of
                    (Node _ (TypeAnnotation.Typed (Node _ ( [], "Never" )) [])) :: [] ->
                        True

                    (Node _ (TypeAnnotation.Typed (Node _ ( [ "Basics" ], "Never" )) [])) :: [] ->
                        True

                    _ ->
                        False

            else
                False

        _ ->
            False



-- EXPRESSION VISITOR


expressionVisitor : Node Expression -> ModuleContext -> ( List nothing, ModuleContext )
expressionVisitor node moduleContext =
    let
        newModuleContext : ModuleContext
        newModuleContext =
            case List.head moduleContext.ignoreBlocks of
                Just expressionsWhereToIgnoreCases ->
                    case RangeDict.get (Node.range node) expressionsWhereToIgnoreCases of
                        Just constructorsToIgnore ->
                            { moduleContext | constructorsToIgnore = constructorsToIgnore :: moduleContext.constructorsToIgnore }

                        Nothing ->
                            moduleContext

                Nothing ->
                    moduleContext
    in
    expressionVisitorHelp node newModuleContext


expressionExitVisitor : Node Expression -> ModuleContext -> ( List nothing, ModuleContext )
expressionExitVisitor node moduleContext =
    let
        newModuleContext : ModuleContext
        newModuleContext =
            case Node.value node of
                Expression.CaseExpression _ ->
                    { moduleContext | ignoreBlocks = List.drop 1 moduleContext.ignoreBlocks }

                _ ->
                    moduleContext
    in
    case List.head newModuleContext.ignoreBlocks of
        Just rangesWhereToIgnoreConstructors ->
            if RangeDict.member (Node.range node) rangesWhereToIgnoreConstructors then
                ( []
                , { newModuleContext | constructorsToIgnore = List.drop 1 newModuleContext.constructorsToIgnore }
                )

            else
                ( [], newModuleContext )

        Nothing ->
            ( [], newModuleContext )


expressionVisitorHelp : Node Expression -> ModuleContext -> ( List nothing, ModuleContext )
expressionVisitorHelp node moduleContext =
    case Node.value node of
        Expression.FunctionOrValue _ name ->
            case ModuleNameLookupTable.moduleNameFor moduleContext.lookupTable node of
                Just moduleName ->
                    ( [], registerUsedFunctionOrValue (Node.range node) moduleName name moduleContext )

                Nothing ->
                    ( [], moduleContext )

        Expression.OperatorApplication operator _ left right ->
            if operator == "==" || operator == "/=" then
                let
                    ranges : List Range
                    ranges =
                        List.concatMap staticRanges [ left, right ]
                in
                ( [], { moduleContext | ignoredComparisonRanges = ranges ++ moduleContext.ignoredComparisonRanges } )

            else
                ( [], moduleContext )

        Expression.LetExpression { declarations } ->
            ( []
            , declarations
                |> List.filterMap
                    (\declaration ->
                        case Node.value declaration of
                            Expression.LetFunction function ->
                                Just (Maybe.map (Node.value >> .typeAnnotation) function.signature)

                            Expression.LetDestructuring _ _ ->
                                Nothing
                    )
                |> List.foldl markPhantomTypesFromTypeAnnotationAsUsed moduleContext
            )

        Expression.CaseExpression { cases } ->
            let
                casesAndConstructors : List ( { bodyRange : Range, nodeRange : Range }, Set ( ModuleName, String ) )
                casesAndConstructors =
                    List.map
                        (\( pattern, body ) ->
                            ( { bodyRange = Node.range body
                              , nodeRange =
                                    { start = (Node.range pattern).start
                                    , end = (Node.range body).end
                                    }
                              }
                            , constructorsInPattern moduleContext.lookupTable pattern
                            )
                        )
                        cases

                newCases : RangeDict (Set ( ModuleName, String ))
                newCases =
                    casesAndConstructors
                        |> List.map (Tuple.mapFirst .bodyRange)
                        |> RangeDict.fromList

                constructorsAndRangesToRemove : List ( Range, ( ModuleNameAsString, ConstructorName ) )
                constructorsAndRangesToRemove =
                    List.concatMap
                        (\( ranges, constructors ) ->
                            constructors
                                |> Set.toList
                                |> List.map (\( moduleName, constructorName ) -> ( ranges.nodeRange, ( String.join "." moduleName, constructorName ) ))
                        )
                        casesAndConstructors
            in
            ( []
            , { moduleContext
                | ignoreBlocks = newCases :: moduleContext.ignoreBlocks
                , fixesForRemovingConstructor =
                    List.foldl
                        (\( nodeRange, constructor ) acc ->
                            Dict.update
                                constructor
                                (Maybe.withDefault [] >> (\list -> Just (Fix.removeRange nodeRange :: list)))
                                acc
                        )
                        moduleContext.fixesForRemovingConstructor
                        constructorsAndRangesToRemove
              }
            )

        _ ->
            ( [], moduleContext )


forOne : ( Node Pattern, Node a ) -> ModuleNameLookupTable -> { ignoreBlock : ( Range, Set ( ModuleName, String ) ), fixes : Dict ( ModuleNameAsString, ConstructorName ) Fix }
forOne ( pattern, body ) lookupTable =
    let
        constructors : Set ( ModuleName, String )
        constructors =
            constructorsInPattern lookupTable pattern

        fixes : Dict ( ModuleNameAsString, ConstructorName ) Fix
        fixes =
            List.foldl
                (\( moduleName, constructorName ) acc ->
                    Dict.insert
                        ( String.join "." moduleName, constructorName )
                        (Fix.removeRange
                            { start = (Node.range pattern).start
                            , end = (Node.range body).end
                            }
                        )
                        acc
                )
                Dict.empty
                (Set.toList constructors)
    in
    { ignoreBlock = ( Node.range body, constructors )
    , fixes = fixes
    }


staticRanges : Node Expression -> List Range
staticRanges node =
    case Node.value node of
        Expression.FunctionOrValue _ _ ->
            [ Node.range node ]

        Expression.Application ((Node _ (Expression.FunctionOrValue _ name)) :: restOfArgs) ->
            if isCapitalized name then
                Node.range node :: List.concatMap staticRanges restOfArgs

            else
                []

        Expression.OperatorApplication operator _ left right ->
            if List.member operator [ "+", "-", "==", "/=" ] then
                List.concatMap staticRanges [ left, right ]

            else
                []

        Expression.ListExpr nodes ->
            List.concatMap staticRanges nodes

        Expression.TupledExpression nodes ->
            List.concatMap staticRanges nodes

        Expression.ParenthesizedExpression expr ->
            staticRanges expr

        Expression.RecordExpr fields ->
            List.concatMap (Node.value >> Tuple.second >> staticRanges) fields

        Expression.RecordUpdateExpression _ fields ->
            List.concatMap (Node.value >> Tuple.second >> staticRanges) fields

        Expression.RecordAccess expr _ ->
            staticRanges expr

        _ ->
            []


constructorsInPattern : ModuleNameLookupTable -> Node Pattern -> Set ( ModuleName, String )
constructorsInPattern lookupTable node =
    case Node.value node of
        Pattern.NamedPattern qualifiedNameRef patterns ->
            let
                initialSet : Set ( ModuleName, String )
                initialSet =
                    case ModuleNameLookupTable.moduleNameFor lookupTable node of
                        Just realModuleName ->
                            Set.fromList [ ( realModuleName, qualifiedNameRef.name ) ]

                        Nothing ->
                            Set.empty
            in
            List.foldl (\pattern acc -> Set.union (constructorsInPattern lookupTable pattern) acc) initialSet patterns

        Pattern.TuplePattern patterns ->
            List.foldl (\pattern acc -> Set.union (constructorsInPattern lookupTable pattern) acc) Set.empty patterns

        Pattern.UnConsPattern left right ->
            Set.union
                (constructorsInPattern lookupTable left)
                (constructorsInPattern lookupTable right)

        Pattern.ListPattern patterns ->
            List.foldl (\pattern acc -> Set.union (constructorsInPattern lookupTable pattern) acc) Set.empty patterns

        Pattern.AsPattern pattern _ ->
            constructorsInPattern lookupTable pattern

        Pattern.ParenthesizedPattern pattern ->
            constructorsInPattern lookupTable pattern

        _ ->
            Set.empty


registerUsedFunctionOrValue : Range -> ModuleName -> ConstructorName -> ModuleContext -> ModuleContext
registerUsedFunctionOrValue range moduleName name moduleContext =
    if not (isCapitalized name) then
        moduleContext

    else if List.member range moduleContext.ignoredComparisonRanges then
        { moduleContext
            | wasUsedInComparisons =
                Set.insert
                    ( String.join "." moduleName, name )
                    moduleContext.wasUsedInComparisons
        }

    else if List.any (Set.member ( moduleName, name )) moduleContext.constructorsToIgnore then
        { moduleContext | wasUsedInLocationThatNeedsItself = Set.insert ( String.join "." moduleName, name ) moduleContext.wasUsedInLocationThatNeedsItself }

    else
        { moduleContext
            | usedFunctionsOrValues =
                insertIntoUsedFunctionsOrValues
                    ( moduleName, name )
                    moduleContext.usedFunctionsOrValues
        }


isCapitalized : String -> Bool
isCapitalized name =
    case String.uncons name of
        Just ( char, _ ) ->
            Char.isUpper char

        Nothing ->
            False



-- FINAL PROJECT EVALUATION


finalProjectEvaluation : ProjectContext -> List (Error { useErrorForModule : () })
finalProjectEvaluation projectContext =
    projectContext.declaredConstructors
        |> Dict.toList
        |> List.concatMap
            (\( moduleName, ExposedConstructors { moduleKey, customTypes } ) ->
                let
                    usedConstructors : Set ConstructorName
                    usedConstructors =
                        Dict.get moduleName projectContext.usedConstructors
                            |> Maybe.withDefault Set.empty
                in
                customTypes
                    |> Dict.values
                    |> List.concatMap
                        (\constructors ->
                            constructors
                                |> Dict.filter (\constructorName _ -> not <| Set.member constructorName usedConstructors)
                                |> Dict.values
                                |> List.map
                                    (\constructorInformation ->
                                        errorForModule
                                            moduleKey
                                            { wasUsedInLocationThatNeedsItself = Set.member ( moduleName, constructorInformation.name ) projectContext.wasUsedInLocationThatNeedsItself
                                            , wasUsedInComparisons = Set.member ( moduleName, constructorInformation.name ) projectContext.wasUsedInComparisons
                                            , isUsedInOtherModules = Set.member ( moduleName, constructorInformation.name ) projectContext.wasUsedInOtherModules
                                            , fixesForRemovingConstructor = Dict.get ( moduleName, constructorInformation.name ) projectContext.fixesForRemovingConstructor |> Maybe.withDefault []
                                            }
                                            constructorInformation
                                    )
                        )
            )



-- ERROR


errorInformation : { wasUsedInLocationThatNeedsItself : Bool, wasUsedInComparisons : Bool } -> String -> { message : String, details : List String }
errorInformation { wasUsedInLocationThatNeedsItself, wasUsedInComparisons } name =
    { message = "Type constructor `" ++ name ++ "` is not used."
    , details =
        [ ( defaultDetails, True )
        , ( "I found it used in comparisons, but since it is never created anywhere, all of those can be evaluated to False (for (==), True for (/=)).", wasUsedInComparisons )
        , ( "The only locations where I found it being created require already having one.", wasUsedInLocationThatNeedsItself )
        ]
            |> List.filter Tuple.second
            |> List.map Tuple.first
    }


defaultDetails : String
defaultDetails =
    "This type constructor is never used. It might be handled everywhere it might appear, but there is no location where this value actually gets created."


errorForModule :
    Rule.ModuleKey
    ->
        { wasUsedInLocationThatNeedsItself : Bool
        , wasUsedInComparisons : Bool
        , isUsedInOtherModules : Bool
        , fixesForRemovingConstructor : List Fix
        }
    -> ConstructorInformation
    -> Error scope
errorForModule moduleKey params constructorInformation =
    Rule.errorForModuleWithFix
        moduleKey
        (errorInformation
            { wasUsedInLocationThatNeedsItself = params.wasUsedInLocationThatNeedsItself
            , wasUsedInComparisons = params.wasUsedInComparisons
            }
            constructorInformation.name
        )
        constructorInformation.rangeToReport
        (case constructorInformation.rangeToRemove of
            Just rangeToRemove ->
                if params.isUsedInOtherModules then
                    []

                else
                    Fix.removeRange rangeToRemove :: params.fixesForRemovingConstructor

            Nothing ->
                []
        )



-- TYPE ANNOTATION UTILITY FUNCTIONS


markPhantomTypesFromTypeAnnotationAsUsed : Maybe (Node TypeAnnotation) -> ModuleContext -> ModuleContext
markPhantomTypesFromTypeAnnotationAsUsed maybeTypeAnnotation moduleContext =
    let
        used : List ( ModuleName, CustomTypeName )
        used =
            case maybeTypeAnnotation of
                Just typeAnnotation ->
                    collectTypesUsedAsPhantomVariables
                        moduleContext
                        moduleContext.phantomVariables
                        typeAnnotation

                Nothing ->
                    []

        usedFunctionsOrValues : Dict ModuleNameAsString (Set ConstructorName)
        usedFunctionsOrValues =
            List.foldl
                insertIntoUsedFunctionsOrValues
                moduleContext.usedFunctionsOrValues
                used
    in
    { moduleContext | usedFunctionsOrValues = usedFunctionsOrValues }


insertIntoUsedFunctionsOrValues : ( ModuleName, ConstructorName ) -> Dict ModuleNameAsString (Set ConstructorName) -> Dict ModuleNameAsString (Set ConstructorName)
insertIntoUsedFunctionsOrValues ( moduleName, constructorName ) dict =
    Dict.update
        (String.join "." moduleName)
        (\maybeSet ->
            case maybeSet of
                Just set ->
                    Just (Set.insert constructorName set)

                Nothing ->
                    Just (Set.singleton constructorName)
        )
        dict


collectGenericsFromTypeAnnotation : Node TypeAnnotation -> List String
collectGenericsFromTypeAnnotation node =
    case Node.value node of
        TypeAnnotation.FunctionTypeAnnotation a b ->
            collectGenericsFromTypeAnnotation a ++ collectGenericsFromTypeAnnotation b

        TypeAnnotation.Typed _ params ->
            List.concatMap collectGenericsFromTypeAnnotation params

        TypeAnnotation.Record list ->
            list
                |> List.concatMap (Node.value >> Tuple.second >> collectGenericsFromTypeAnnotation)

        TypeAnnotation.GenericRecord _ list ->
            Node.value list
                |> List.concatMap (Node.value >> Tuple.second >> collectGenericsFromTypeAnnotation)

        TypeAnnotation.Tupled list ->
            List.concatMap collectGenericsFromTypeAnnotation list

        TypeAnnotation.GenericType var ->
            [ var ]

        TypeAnnotation.Unit ->
            []


collectTypesUsedAsPhantomVariables : ModuleContext -> Dict ModuleName (List ( CustomTypeName, Int )) -> Node TypeAnnotation -> List ( ModuleName, CustomTypeName )
collectTypesUsedAsPhantomVariables moduleContext phantomVariables node =
    case Node.value node of
        TypeAnnotation.FunctionTypeAnnotation a b ->
            collectTypesUsedAsPhantomVariables moduleContext phantomVariables a
                ++ collectTypesUsedAsPhantomVariables moduleContext phantomVariables b

        TypeAnnotation.Typed (Node.Node typeRange ( _, name )) params ->
            let
                moduleNameOfPhantomContainer : ModuleName
                moduleNameOfPhantomContainer =
                    ModuleNameLookupTable.moduleNameAt moduleContext.lookupTable typeRange
                        |> Maybe.withDefault []

                typesUsedInThePhantomVariablePosition : List ( ModuleName, CustomTypeName )
                typesUsedInThePhantomVariablePosition =
                    Dict.get moduleNameOfPhantomContainer phantomVariables
                        |> Maybe.withDefault []
                        |> List.filter (\( type_, _ ) -> type_ == name)
                        |> List.filterMap
                            (\( _, index ) ->
                                case listAtIndex index params |> Maybe.map Node.value of
                                    Just (TypeAnnotation.Typed (Node.Node subTypeRange ( _, typeName )) _) ->
                                        ModuleNameLookupTable.moduleNameAt moduleContext.lookupTable subTypeRange
                                            |> Maybe.map (\moduleNameOfPhantomVariable -> ( moduleNameOfPhantomVariable, typeName ))

                                    _ ->
                                        Nothing
                            )
            in
            List.concat
                [ typesUsedInThePhantomVariablePosition
                , List.concatMap (collectTypesUsedAsPhantomVariables moduleContext phantomVariables) params
                ]

        TypeAnnotation.Record list ->
            list
                |> List.concatMap (Node.value >> Tuple.second >> collectTypesUsedAsPhantomVariables moduleContext phantomVariables)

        TypeAnnotation.GenericRecord _ list ->
            Node.value list
                |> List.concatMap (Node.value >> Tuple.second >> collectTypesUsedAsPhantomVariables moduleContext phantomVariables)

        TypeAnnotation.Tupled list ->
            List.concatMap (collectTypesUsedAsPhantomVariables moduleContext phantomVariables) list

        TypeAnnotation.GenericType _ ->
            []

        TypeAnnotation.Unit ->
            []


listAtIndex : Int -> List a -> Maybe a
listAtIndex index list =
    case ( index, list ) of
        ( 0, a :: [] ) ->
            Just a

        ( _, [] ) ->
            Nothing

        ( n, _ :: rest ) ->
            listAtIndex (n - 1) rest


mapDictKeys : (comparable -> comparable1) -> Dict comparable v -> Dict comparable1 v
mapDictKeys keyMapper dict =
    Dict.foldl
        (\key value acc -> Dict.insert (keyMapper key) value acc)
        Dict.empty
        dict
