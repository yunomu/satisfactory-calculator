port module Main exposing (main)

import Browser
import Browser.Dom
import Element exposing (Element)
import Element.Lazy as Lazy
import Html exposing (Html)
import Html.Lazy
import Http
import Json.Decode as JD
import Json.Encode as JE
import Recipe
import Task
import Time
import View


port scrollY : (Int -> msg) -> Sub msg


port saveRecipes : String -> Cmd msg


port loadRecipes : (String -> msg) -> Sub msg


type alias Flags =
    { recipes : String
    , items : String
    }


type alias SavedRecipe =
    { name : String
    , recipes : List View.RecipeView
    , timestamp : Int
    }


type alias Model =
    { flags : Flags
    , viewModel : View.Model
    , savedRecipes : List SavedRecipe
    }


type Msg
    = FetchRecipesCSV (Result Http.Error String)
    | FetchItemsCSV String (Result Http.Error String)
    | ViewMsg View.Msg
    | OnScrollY Int
    | LoadedRecipes String
    | SaveButtonClicked
    | SaveCurrentRecipes Time.Posix
    | LoadSavedRecipe Int
    | DeleteSavedRecipe Int


httpGet : (Result Http.Error String -> msg) -> String -> Cmd msg
httpGet toMsg url =
    Http.get
        { url = url
        , expect = Http.expectString toMsg
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { flags = flags
      , viewModel = View.empty
      , savedRecipes = []
      }
    , Cmd.batch
        [ httpGet FetchRecipesCSV flags.recipes
        ]
    )


httpError : Http.Error -> String
httpError e =
    "HTTP ERROR"


httpAndThen : (String -> Result String a) -> Result Http.Error String -> Result String a
httpAndThen b res =
    Result.andThen b (Result.mapError httpError res)


encodeRecipeView : View.RecipeView -> JE.Value
encodeRecipeView rv =
    JE.object
        [ ( "item", JE.string rv.item )
        , ( "multiple", JE.float rv.multiple )
        , ( "selectedName", JE.string rv.selected.name )
        , ( "direction", JE.string <| directionToString rv.direction )
        ]


directionToString : View.Direction -> String
directionToString dir =
    case dir of
        View.Produce ->
            "Produce"

        View.Consume ->
            "Consume"

        View.None ->
            "None"


stringToDirection : String -> View.Direction
stringToDirection s =
    case s of
        "Produce" ->
            View.Produce

        "Consume" ->
            View.Consume

        _ ->
            View.None


encodeSavedRecipe : SavedRecipe -> JE.Value
encodeSavedRecipe sr =
    JE.object
        [ ( "name", JE.string sr.name )
        , ( "recipes", JE.list encodeRecipeView sr.recipes )
        , ( "timestamp", JE.int sr.timestamp )
        ]


encodeSavedRecipes : List SavedRecipe -> String
encodeSavedRecipes recipes =
    JE.encode 0 (JE.list encodeSavedRecipe recipes)


decodeRecipeView : JE.Value -> Maybe View.RecipeView
decodeRecipeView value =
    JD.decodeValue
        (JD.map4
            (\item multiple selectedName direction ->
                { item = item
                , recipes = []
                , multiple = multiple
                , selected = Recipe.Recipe selectedName "" 0 [] []
                , direction = stringToDirection direction
                }
            )
            (JD.field "item" JD.string)
            (JD.field "multiple" JD.float)
            (JD.field "selectedName" JD.string)
            (JD.field "direction" JD.string)
        )
        value
        |> Result.toMaybe


decodeSavedRecipe : JE.Value -> Maybe SavedRecipe
decodeSavedRecipe value =
    JD.decodeValue
        (JD.map3 SavedRecipe
            (JD.field "name" JD.string)
            (JD.field "recipes" (JD.list (JD.map (\v -> v) JD.value) |> JD.map (List.filterMap decodeRecipeView)))
            (JD.field "timestamp" JD.int)
        )
        value
        |> Result.toMaybe


decodeSavedRecipes : String -> List SavedRecipe
decodeSavedRecipes str =
    case JD.decodeString (JD.list JD.value) str of
        Ok values ->
            List.filterMap decodeSavedRecipe values

        Err _ ->
            []


restoreRecipes : View.Model -> List View.RecipeView -> List View.RecipeView
restoreRecipes model loadedRecipes =
    let
        restoreRecipe rv =
            case rv.direction of
                View.Produce ->
                    let
                        recipes =
                            Lib.getDefault [] rv.item model.productMap

                        selected =
                            List.find (\r -> r.name == rv.selected.name) recipes
                                |> Maybe.withDefault (Recipe.Recipe "" "" 0 [] [])
                    in
                    { rv | recipes = recipes, selected = selected }

                View.Consume ->
                    let
                        recipes =
                            Lib.getDefault [] rv.item model.sourceMap

                        selected =
                            List.find (\r -> r.name == rv.selected.name) recipes
                                |> Maybe.withDefault (Recipe.Recipe "" "" 0 [] [])
                    in
                    { rv | recipes = recipes, selected = selected }

                View.None ->
                    rv
    in
    List.map restoreRecipe loadedRecipes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchRecipesCSV res ->
            case res of
                Ok recipesCsv ->
                    ( model
                    , httpGet (FetchItemsCSV recipesCsv) model.flags.items
                    )

                Err err ->
                    ( model, Cmd.none )

        FetchItemsCSV recipesCsv res ->
            case httpAndThen (Recipe.build recipesCsv) res of
                Ok recipes ->
                    let
                        ( viewModel, cmd ) =
                            View.init ViewMsg recipes
                    in
                    ( { model
                        | viewModel = viewModel
                      }
                    , Cmd.batch [ cmd, loadRecipes "" ]
                    )

                Err error ->
                    ( model, Cmd.none )

        ViewMsg viewMsg ->
            let
                ( viewModel, cmd ) =
                    View.update ViewMsg viewMsg model.viewModel
            in
            ( { model | viewModel = viewModel }
            , cmd
            )

        OnScrollY y ->
            let
                ( viewModel, cmd ) =
                    View.update ViewMsg (View.OnScrollY y) model.viewModel
            in
            ( { model | viewModel = viewModel }
            , cmd
            )

        LoadedRecipes jsonStr ->
            if String.isEmpty jsonStr then
                ( model, Cmd.none )

            else
                let
                    loadedRecipes = decodeSavedRecipes jsonStr
                in
                ( { model | savedRecipes = loadedRecipes }, Cmd.none )

        SaveButtonClicked ->
            ( model, Task.perform SaveCurrentRecipes Time.now )

        SaveCurrentRecipes timestamp ->
            let
                restoredRecipes =
                    restoreRecipes model.viewModel model.viewModel.recipes

                recipeName =
                    case List.head restoredRecipes of
                        Just rv ->
                            rv.item

                        Nothing ->
                            "Unnamed"

                newSavedRecipe =
                    SavedRecipe recipeName restoredRecipes (Time.posixToMillis timestamp)

                updatedSavedRecipes =
                    newSavedRecipe :: model.savedRecipes

                newModel =
                    { model | savedRecipes = updatedSavedRecipes }
            in
            ( newModel, saveRecipes (encodeSavedRecipes updatedSavedRecipes) )

        LoadSavedRecipe timestamp ->
            case List.find (\sr -> sr.timestamp == timestamp) model.savedRecipes of
                Just savedRecipe ->
                    let
                        restoredRecipes =
                            restoreRecipes model.viewModel savedRecipe.recipes

                        oldModel = model.viewModel
                        newViewModel = View.restoreRecipes oldModel restoredRecipes
                    in
                    ( { model | viewModel = newViewModel }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        DeleteSavedRecipe timestamp ->
            let
                updatedSavedRecipes =
                    List.filter (\sr -> sr.timestamp /= timestamp) model.savedRecipes

                newModel =
                    { model | savedRecipes = updatedSavedRecipes }
            in
            ( newModel, saveRecipes (encodeSavedRecipes updatedSavedRecipes) )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ scrollY OnScrollY
        , loadRecipes LoadedRecipes
        ]


view_ : View.Model -> List SavedRecipe -> Element Msg
view_ viewModel savedRecipes =
    Element.column
        [ Element.spacing 20, Element.padding 10 ]
        [ View.view ViewMsg viewModel
        , Element.column
            [ Element.spacing 10, Element.padding 10 ]
            [ Element.row [ Element.spacing 10 ]
                [ Element.text "保存済みレシピ"
                , Element.Input.button
                    [ Element.padding 5 ]
                    { onPress = Just SaveButtonClicked
                    , label = Element.text "保存"
                    }
                ]
            , Element.column
                [ Element.spacing 5 ]
                (List.map
                    (\sr ->
                        Element.row
                            [ Element.spacing 10 ]
                            [ Element.text sr.name
                            , Element.Input.button
                                [ Element.padding 5 ]
                                { onPress = Just (LoadSavedRecipe sr.timestamp)
                                , label = Element.text "読込"
                                }
                            , Element.Input.button
                                [ Element.padding 5 ]
                                { onPress = Just (DeleteSavedRecipe sr.timestamp)
                                , label = Element.text "削除"
                                }
                            ]
                    )
                    savedRecipes
                )
            ]
        ]


view : Model -> Html Msg
view model =
    Element.layout [] <|
        view_ model.viewModel model.savedRecipes


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = Html.Lazy.lazy view
        , update = update
        , subscriptions = subscriptions
        }
