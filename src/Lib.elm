module Lib exposing
    ( find
    , getDefault
    , maybe
    , perform
    )

import Dict exposing (Dict)
import Task


perform : msg -> Cmd msg
perform =
    Task.perform identity << Task.succeed


maybe : b -> (a -> b) -> Maybe a -> b
maybe b f =
    Maybe.withDefault b << Maybe.map f


find : (a -> Bool) -> List a -> Maybe a
find f list =
    case list of
        x :: xs ->
            if f x then
                Just x

            else
                find f xs

        [] ->
            Nothing


getDefault : v -> comparable -> Dict comparable v -> v
getDefault default k =
    Maybe.withDefault default << Dict.get k
