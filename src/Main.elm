port module Main exposing (main)

import Browser
import Browser.Dom
import Element exposing (Element)
import Element.Lazy as Lazy
import Html exposing (Html)
import Html.Lazy
import Http
import Recipe
import View


port scrollY : (Int -> msg) -> Sub msg


type alias Flags =
    { recipes : String
    , items : String
    }


type alias Model =
    { flags : Flags
    , viewModel : View.Model
    }


type Msg
    = FetchRecipesCSV (Result Http.Error String)
    | FetchItemsCSV String (Result Http.Error String)
    | ViewMsg View.Msg
    | OnScrollY Int


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
                    , cmd
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


subscriptions : Model -> Sub Msg
subscriptions model =
    scrollY OnScrollY


view_ : View.Model -> Element Msg
view_ =
    View.view ViewMsg


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Lazy.lazy view_ model.viewModel


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = Html.Lazy.lazy view
        , update = update
        , subscriptions = subscriptions
        }
