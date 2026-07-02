module Recipe exposing
    ( Item
    , ProcessType(..)
    , Recipe
    , build
    , perMin
    , summary
    )

import Csv.Builder as Builder exposing (Builder)
import Dict exposing (Dict)
import Lib


type ProcessType
    = Extract
    | Produce
    | Consume
    | ProcessTypeError


toProcessType : String -> ProcessType
toProcessType s =
    case s of
        "生産" ->
            Extract

        "制作" ->
            Produce

        "消費" ->
            Consume

        _ ->
            ProcessTypeError


pTypeString : ProcessType -> String
pTypeString t =
    case t of
        Extract ->
            "生産"

        Produce ->
            "制作"

        Consume ->
            "消費"

        ProcessTypeError ->
            ""


type alias Item =
    { name : String
    , amount : Float
    , process : ProcessType
    }


type alias Recipe =
    { name : String
    , equip : String
    , duration : Float
    , input : List Item
    , output : List Item
    }


recipeBuilder : Builder Recipe
recipeBuilder =
    Builder.fmap (\name equip duration -> Recipe name equip duration [] [])
        Builder.string
        |> Builder.apply_ Builder.string
        |> Builder.apply_ Builder.float


itemsBuilder : Builder ( String, Item )
itemsBuilder =
    Builder.fmap (\recipe root name amount -> ( recipe, Item name amount root ))
        Builder.string
        |> Builder.apply_ (Builder.fmap toProcessType Builder.string)
        |> Builder.apply_ Builder.string
        |> Builder.apply_ Builder.float


construct : List Recipe -> List ( String, Item ) -> List Recipe
construct recipes =
    let
        setItem item r =
            case item.process of
                Extract ->
                    { r | output = item :: r.output }

                Produce ->
                    { r | output = item :: r.output }

                Consume ->
                    { r | input = item :: r.input }

                _ ->
                    r

        f map is =
            case is of
                ( name, item ) :: items ->
                    f (Dict.update name (Maybe.map <| setItem item) map) items

                [] ->
                    map
    in
    Dict.values << f (Dict.fromList <| List.map (\r -> ( r.name, r )) recipes)


sortRecipes : List Recipe -> List Recipe
sortRecipes =
    let
        f a =
            ( String.startsWith "代替:" a.name, String.length a.name )
    in
    List.sortWith <|
        \a b ->
            case ( f a, f b ) of
                ( ( False, _ ), ( True, _ ) ) ->
                    LT

                ( ( True, _ ), ( False, _ ) ) ->
                    GT

                ( ( _, la ), ( _, lb ) ) ->
                    if la < lb then
                        LT

                    else if la > lb then
                        GT

                    else
                        compare a.name b.name


mkProductMap : List Recipe -> Dict String (List Recipe)
mkProductMap =
    Dict.map (\k v -> sortRecipes v)
        << List.foldl
            (\dicta dictb ->
                Dict.merge
                    (\k a -> Dict.insert k a)
                    (\k a b -> Dict.insert k (a ++ b))
                    (\k b -> Dict.insert k b)
                    dicta
                    dictb
                    Dict.empty
            )
            Dict.empty
        << List.concatMap (\r -> List.map (\o -> Dict.singleton o.name [ r ]) r.output)


build : String -> String -> Result String (Dict String (List Recipe))
build recipesCsv itemsCsv =
    Result.map2 construct
        (Builder.build recipeBuilder recipesCsv)
        (Builder.build itemsBuilder itemsCsv)
        |> Result.map mkProductMap


perMin : Float -> Float -> Float
perMin amount duration =
    amount * 60 / duration


addItem : Item -> Item -> Item
addItem a b =
    { a | amount = a.amount + b.amount }


mergeItems : List Item -> List Item -> List Item
mergeItems items1 =
    let
        f dict items =
            case items of
                i :: is ->
                    f
                        (case Dict.get i.name dict of
                            Just o ->
                                Dict.insert i.name (addItem i o) dict

                            Nothing ->
                                Dict.insert i.name i dict
                        )
                        is

                [] ->
                    Dict.values dict
    in
    f (Dict.fromList <| List.map (\i -> ( i.name, i )) items1)


summary : List ( Recipe, Float ) -> ( List Item, List Item )
summary recipes =
    let
        input =
            List.map (\( r, m ) -> List.map (\i -> { i | amount = perMin i.amount r.duration * m }) r.input) recipes
                |> List.foldl mergeItems []

        output =
            List.map (\( r, m ) -> List.map (\i -> { i | amount = perMin i.amount r.duration * m }) r.output) recipes
                |> List.foldl mergeItems []
    in
    mergeItems output (List.map (\i -> { i | amount = negate i.amount }) input)
        |> List.filter (\i -> i.amount /= 0)
        |> List.partition (\i -> i.amount > 0)
        |> Tuple.mapSecond (List.map (\i -> { i | amount = negate i.amount }))
