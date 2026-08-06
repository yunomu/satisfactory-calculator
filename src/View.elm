module View exposing
    ( Model
    , Msg(..)
    , empty
    , init
    , update
    , view
    )

import Dict exposing (Dict)
import Element exposing (Attribute, Element, px)
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Element.Lazy as Lazy
import Html
import Html.Attributes as Attr
import Html.Events
import Json.Decode as JD exposing (Decoder)
import Lib
import Recipe exposing (Item, Recipe)
import Set
import Task
import View.Atom.Button as B


type Direction
    = Produce
    | Consume
    | None


type Mode
    = RecipeMode
    | GraphMode


type Msg
    = SelectProduct Int Direction String
    | SelectRecipe Int String
    | ChangeMultiple Int Float
    | ChangeDirection Int Direction
    | AddRecipe Direction String
    | RemoveRecipe Int
    | ClearRecipes
    | OnScrollY Int
    | NopRenderGraph String String
    | UpdateRecipes
    | ChangeMode Mode


type alias RecipeView =
    { item : String
    , recipes : List Recipe
    , multiple : Float
    , selected : Recipe
    , direction : Direction
    }


graphId : String
graphId =
    "mermaid-container"


errorRecipe : Recipe
errorRecipe =
    Recipe "" "" 0 [] []


type alias Model msg =
    { products : List String
    , productMap : Dict String (List Recipe)
    , sourceMap : Dict String (List Recipe)
    , recipes : List RecipeView
    , scrollY : Int
    , toMsg : Msg -> msg
    , renderGraph : String -> String -> msg
    , mode : Mode
    , input : List Item
    , output : List Item
    }


empty : (Msg -> msg) -> Model msg
empty toMsg =
    { products = []
    , productMap = Dict.empty
    , sourceMap = Dict.empty
    , recipes = []
    , scrollY = 0
    , toMsg = toMsg
    , renderGraph = \id -> toMsg << NopRenderGraph id
    , mode = RecipeMode
    , input = []
    , output = []
    }


init :
    { toMsg : Msg -> msg
    , renderGraph : String -> String -> msg
    }
    -> List Recipe
    -> ( Model msg, Cmd msg )
init msgs recipes =
    let
        productMap =
            Recipe.mkProductMap recipes

        sourceMap =
            Recipe.mkSourceMap recipes

        products =
            Dict.keys productMap

        selectedProduct =
            List.head products |> Maybe.withDefault ""
    in
    ( { products = products
      , productMap = productMap
      , sourceMap = sourceMap
      , recipes = []
      , scrollY = 0
      , toMsg = msgs.toMsg
      , renderGraph = msgs.renderGraph
      , mode = RecipeMode
      , input = []
      , output = []
      }
    , Lib.perform <| msgs.toMsg <| AddRecipe Produce selectedProduct
    )


maybeCmd : (a -> Cmd msg) -> Maybe a -> Cmd msg
maybeCmd =
    Lib.maybe Cmd.none


updateIndex : Int -> (a -> a) -> List a -> List a
updateIndex i f list =
    case ( i, list ) of
        ( 0, x :: xs ) ->
            f x :: xs

        ( _, x :: xs ) ->
            x :: updateIndex (i - 1) f xs

        ( _, [] ) ->
            []


removeIndex : Int -> List a -> List a
removeIndex i list =
    case ( i, list ) of
        ( 0, x :: xs ) ->
            xs

        ( _, x :: xs ) ->
            x :: removeIndex (i - 1) xs

        ( _, [] ) ->
            []


update : Msg -> Model msg -> ( Model msg, Cmd msg )
update msg model =
    case msg of
        SelectProduct idx dir product ->
            let
                map =
                    case dir of
                        Produce ->
                            model.productMap

                        Consume ->
                            model.sourceMap

                        _ ->
                            Dict.empty

                filteredRecipes =
                    Lib.getDefault [] product map
            in
            ( { model
                | recipes =
                    updateIndex idx
                        (\r ->
                            { r
                                | item = product
                                , recipes = filteredRecipes
                                , multiple = 1
                            }
                        )
                        model.recipes
              }
            , maybeCmd (Lib.perform << model.toMsg << SelectRecipe idx) <|
                Maybe.map (\r -> r.name) <|
                    List.head filteredRecipes
            )

        SelectRecipe idx recipe ->
            ( { model
                | recipes =
                    updateIndex idx
                        (\rv ->
                            { rv
                                | selected =
                                    Lib.find (\r -> r.name == recipe) rv.recipes
                                        |> Maybe.withDefault errorRecipe
                            }
                        )
                        model.recipes
              }
            , Lib.perform <| model.toMsg UpdateRecipes
            )

        ChangeMultiple idx s ->
            ( { model
                | recipes = updateIndex idx (\r -> { r | multiple = r.multiple + s }) model.recipes
              }
            , Lib.perform <| model.toMsg UpdateRecipes
            )

        AddRecipe direction product ->
            ( { model
                | recipes =
                    model.recipes ++ [ RecipeView product [] 1 errorRecipe direction ]
              }
            , Lib.perform <| model.toMsg <| ChangeDirection (List.length model.recipes) direction
            )

        ChangeDirection idx dir ->
            ( { model
                | recipes =
                    updateIndex idx
                        (\r ->
                            let
                                map =
                                    case dir of
                                        Produce ->
                                            model.productMap

                                        Consume ->
                                            model.sourceMap

                                        _ ->
                                            Dict.empty

                                rs =
                                    Lib.getDefault [] r.item map

                                selected =
                                    List.head rs
                                        |> Maybe.withDefault errorRecipe
                            in
                            { r
                                | direction = dir
                                , selected = selected
                                , recipes = rs
                            }
                        )
                        model.recipes
              }
            , Lib.perform <| model.toMsg UpdateRecipes
            )

        RemoveRecipe idx ->
            ( if List.length model.recipes == 1 then
                model

              else
                { model | recipes = removeIndex idx model.recipes }
            , Lib.perform <| model.toMsg UpdateRecipes
            )

        ClearRecipes ->
            ( { model | recipes = List.take 1 model.recipes }
            , Lib.perform <| model.toMsg UpdateRecipes
            )

        OnScrollY y ->
            ( { model | scrollY = y }
            , Cmd.none
            )

        NopRenderGraph _ _ ->
            ( model, Cmd.none )

        UpdateRecipes ->
            let
                ( output, input ) =
                    List.map (\r -> ( r.selected, r.multiple )) model.recipes
                        |> Recipe.summary
            in
            ( { model
                | input = input
                , output = output
              }
            , Cmd.none
            )

        ChangeMode mode ->
            let
                label =
                    String.filter (\c -> not <| List.member c [ '・', '（', '）', ' ', '(', ')' ])

                items =
                    model.recipes
                        |> List.map (\r -> r.selected)
                        |> List.concatMap (\r -> r.input ++ r.output)
                        |> List.map (\i -> i.name)
                        |> Set.fromList
                        |> Set.toList
                        |> List.map (\i -> String.concat [ label i, "([", i, "])" ])

                equipLabel r =
                    String.concat [ label r.equip, "_", label r.name ]

                equips =
                    model.recipes
                        |> List.map (\r -> ( r.selected, r.multiple ))
                        |> List.map
                            (\( r, mul ) ->
                                String.concat
                                    [ equipLabel r
                                    , "[\""
                                    , r.equip
                                    , "("
                                    , String.fromFloat mul
                                    , ")"
                                    , "\"]"
                                    ]
                            )

                inputEdge eq mul d item =
                    String.concat
                        [ label item.name
                        , "--->|"
                        , String.fromFloat <| Recipe.perMin item.amount d mul
                        , "|"
                        , eq
                        ]

                outputEdge eq mul d item =
                    String.concat
                        [ eq
                        , "--->|"
                        , String.fromFloat <| Recipe.perMin item.amount d mul
                        , "|"
                        , label item.name
                        ]

                procs =
                    model.recipes
                        |> List.map (\r -> ( r.selected, r.multiple ))
                        |> List.concatMap
                            (\( r, mul ) ->
                                let
                                    equip =
                                        equipLabel r
                                in
                                List.map (inputEdge equip mul r.duration) r.input
                                    ++ List.map (outputEdge equip mul r.duration) r.output
                            )

                srcs =
                    model.input
                        |> List.concatMap
                            (\i ->
                                let
                                    item =
                                        label i.name

                                    startLabel =
                                        "start_" ++ item
                                in
                                [ startLabel ++ "(( ))"
                                , String.concat
                                    [ startLabel
                                    , "--->|"
                                    , String.fromFloat i.amount
                                    , "|"
                                    , item
                                    ]
                                ]
                            )

                surplus =
                    model.output
                        |> List.concatMap
                            (\i ->
                                let
                                    item =
                                        label i.name

                                    stopLabel =
                                        "stop_" ++ item
                                in
                                [ stopLabel ++ "((( )))"
                                , String.concat
                                    [ item
                                    , "--->|"
                                    , String.fromFloat i.amount
                                    , "|"
                                    , stopLabel
                                    ]
                                ]
                            )

                graph =
                    String.join "\n" <|
                        "flowchart BT"
                            :: List.concat
                                [ items
                                , equips
                                , procs
                                , surplus
                                , srcs
                                ]
            in
            ( { model | mode = mode }
            , if mode == GraphMode then
                Lib.perform <| model.renderGraph graphId graph

              else
                Cmd.none
            )


selectOnChange : (String -> msg) -> Html.Attribute msg
selectOnChange toMsg =
    Html.Events.on "change" (JD.map toMsg Html.Events.targetValue)


select :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , options : List ( String, String, Bool )
        , label : String
        }
    -> Element msg
select attrs arg =
    Element.row (Element.spacing 5 :: attrs)
        [ Element.text arg.label
        , Element.el [] <|
            Element.html <|
                Html.select [ selectOnChange arg.onChange ] <|
                    List.map
                        (\( value, label, selected ) ->
                            Html.option
                                [ Attr.value value
                                , Attr.selected selected
                                ]
                                [ Html.text label ]
                        )
                        arg.options
        ]


viewItem : (Msg -> msg) -> Direction -> Float -> Float -> Item -> Element msg
viewItem toMsg direction mul duration item =
    Element.row
        [ Element.spacing 5 ]
        [ Element.column
            [ Element.width (px 300)
            , Border.width 1
            , Element.padding 3
            , Element.spacing 3
            ]
            [ Element.text item.name
            , Element.row [ Element.width Element.fill ]
                [ if direction /= None then
                    Element.none

                  else
                    Element.text <| String.fromFloat <| Recipe.round3 <| item.amount * mul
                , Element.el [ Element.alignRight ] <|
                    Element.text <|
                        (String.fromFloat <| Recipe.perMin item.amount duration mul)
                            ++ "/分"
                ]
            ]
        , if direction /= None then
            B.button (toMsg <| AddRecipe direction item.name) "+"

          else
            Element.none
        ]


selectRecipe : (String -> msg) -> String -> List Recipe -> Element msg
selectRecipe selectMsg selected recipes =
    select []
        { onChange = selectMsg
        , options =
            List.map
                (\r ->
                    ( r.name
                    , r.name
                    , r.name == selected
                    )
                )
                recipes
        , label = "レシピ"
        }


viewMul : (Msg -> msg) -> Int -> Float -> Element msg
viewMul toMsg idx mul =
    Element.row
        [ Element.spacing 5
        ]
        [ Element.text "ライン数"
        , Element.row
            [ Element.spacing 3
            ]
            [ B.button (toMsg <| ChangeMultiple idx 1) "+1"
            , Element.el [ Element.width (px 30) ] <|
                Element.el [ Element.alignRight ]
                    (Element.text <| String.fromFloat mul)
            , B.button (toMsg <| ChangeMultiple idx -1) "-1"
            ]
        ]


edges : { bottom : Int, left : Int, right : Int, top : Int }
edges =
    { bottom = 0
    , left = 0
    , right = 0
    , top = 0
    }


ifNone : Bool -> Element msg -> Element msg
ifNone cond elem =
    if cond then
        Element.none

    else
        elem


viewRecipe : (Msg -> msg) -> List String -> Int -> RecipeView -> Element msg
viewRecipe toMsg products idx arg =
    Element.el [ Element.width Element.fill ] <|
        Element.column
            [ Border.widthEach { edges | top = 1 }
            , Element.padding 5
            , Element.width Element.fill
            ]
            [ Element.row [ Element.width Element.fill, Element.spacing 15 ]
                [ Input.radioRow [ Element.spacing 10 ]
                    { onChange = toMsg << ChangeDirection idx
                    , options =
                        [ Input.option Produce <| Element.text "制作"
                        , Input.option Consume <| Element.text "消費"
                        ]
                    , selected = Just arg.direction
                    , label = Input.labelHidden "制作区分"
                    }
                , select []
                    { onChange = toMsg << SelectProduct idx arg.direction
                    , options = List.map (\p -> ( p, p, p == arg.item )) products
                    , label = "アイテム"
                    }
                , Element.el [ Element.alignRight, Element.spacing 3 ] <|
                    B.button (toMsg <| RemoveRecipe idx) "×"
                ]
            , Element.row
                [ Element.width Element.fill
                , Element.spacing 5
                ]
                [ selectRecipe (toMsg << SelectRecipe idx) arg.selected.name arg.recipes
                , Element.el [ Element.alignRight ] <| viewMul toMsg idx arg.multiple
                ]
            , Element.row [ Element.spacing 5 ]
                [ Element.row [ Element.spacing 3 ]
                    [ Element.column [ Element.spacing 2 ] <| List.map (viewItem toMsg None arg.multiple arg.selected.duration) arg.selected.output
                    , Element.text "←"
                    ]
                , Element.column [ Element.width (px 180) ]
                    [ Element.el [ Element.centerX ] <| Element.text arg.selected.equip
                    , Element.el [ Element.centerX ] <| Element.text <| String.fromFloat arg.selected.duration ++ "秒"
                    ]
                , ifNone (List.isEmpty arg.selected.input) <|
                    Element.row [ Element.spacing 3 ]
                        [ Element.text "←"
                        , Element.column [ Element.spacing 2 ] <| List.map (viewItem toMsg None arg.multiple arg.selected.duration) arg.selected.input
                        ]
                ]
            ]


viewRecipes : (Msg -> msg) -> List String -> List RecipeView -> Element msg
viewRecipes toMsg products recipes =
    Element.column
        [ Element.spacing 10
        , Element.alignTop
        ]
        [ Element.row [ Element.width Element.fill ]
            [ Element.text "レシピ"
            , Element.el [ Element.alignRight ] <| B.button (toMsg ClearRecipes) "Clear"
            ]
        , Element.column
            [ Element.padding 5
            , Border.widthEach { edges | bottom = 1 }
            ]
          <|
            List.indexedMap (viewRecipe toMsg products) recipes
        ]


viewIO : (Msg -> msg) -> List Item -> List Item -> Element msg
viewIO toMsg input output =
    Element.column
        [ Element.spacing 10
        ]
        [ Element.text "副産物・必要資源"
        , Element.row [ Element.spacing 15 ]
            [ Element.column [ Element.spacing 2 ] <| List.map (\i -> viewItem toMsg Consume 1 60 i) output
            , Element.text "←"
            , Element.column [ Element.spacing 2 ] <| List.map (\i -> viewItem toMsg Produce 1 60 i) input
            ]
        ]


view : (Msg -> msg) -> Model msg -> Element msg
view toMsg model =
    Element.column
        [ Element.paddingXY 15 10
        , Element.spacing 10
        , Element.width Element.fill
        ]
        [ Element.el [ Border.widthEach { edges | bottom = 1 } ] <|
            Element.link []
                { url = "./"
                , label = Element.text "Satisfactory計算機"
                }
        , Input.radioRow
            [ Element.spacing 10
            ]
            { onChange = model.toMsg << ChangeMode
            , options =
                [ Input.option RecipeMode <| Element.text "レシピ編集"
                , Input.option GraphMode <| Element.text "グラフ表示"
                ]
            , selected = Just model.mode
            , label = Input.labelHidden "表示モード"
            }
        , case model.mode of
            RecipeMode ->
                Element.row
                    [ Element.paddingXY 0 5
                    , Element.spacing 10
                    ]
                    [ Lazy.lazy3 viewRecipes toMsg model.products model.recipes
                    , Element.el
                        [ Element.width Element.fill
                        , Element.paddingEach { edges | top = model.scrollY }
                        , Element.alignTop
                        ]
                      <|
                        Lazy.lazy3 viewIO toMsg model.input model.output
                    ]

            GraphMode ->
                Element.el
                    [ Element.width Element.fill
                    , Element.paddingXY 0 5
                    , Element.spacing 10
                    ]
                <|
                    Element.html <|
                        Html.div [ Attr.id graphId, Attr.style "width" "100%" ] []
        ]
