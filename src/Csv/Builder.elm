module Csv.Builder exposing
    ( Builder
    , apply
    , apply_
    , bind
    , build
    , fail
    , float
    , fmap
    , head
    , int
    , just
    , many
    , maybeFloat
    , maybeInt
    , ok
    , return
    , string
    , stringsN
    )

import Csv.Parser as P
import Parser


type Builder a
    = Builder (List String -> ( Result String a, List String ))


bind : Builder a -> (a -> Builder b) -> Builder b
bind ba bf =
    case ba of
        Builder f ->
            Builder <|
                \ls ->
                    let
                        ( resulta, ls_ ) =
                            f ls
                    in
                    case resulta of
                        Ok a ->
                            case bf a of
                                Builder g ->
                                    g ls_

                        Err err ->
                            ( Err err, ls )


return : a -> Builder a
return a =
    Builder <| \ls -> ( Ok a, ls )


fail : String -> Builder a
fail s =
    Builder <| \ls -> ( Err s, ls )


exec : Builder a -> List String -> Result String a
exec b ls =
    case b of
        Builder f ->
            Tuple.first <| f ls


fmap : (a -> b) -> Builder a -> Builder b
fmap f pa =
    bind pa <| return << f


apply : Builder (a -> b) -> Builder a -> Builder b
apply pf pa =
    bind pf <| \f -> bind pa <| return << f


apply_ : Builder a -> Builder (a -> b) -> Builder b
apply_ pa pf =
    apply pf pa


head : Builder (Maybe String)
head =
    Builder <|
        \ls ->
            case ls of
                [] ->
                    ( Ok Nothing, [] )

                x :: xs ->
                    ( Ok (Just x), xs )


many : Builder a -> Builder (List a)
many b =
    let
        f accm =
            case b of
                Builder g ->
                    Builder <|
                        \ls ->
                            let
                                ( result, ls_ ) =
                                    g ls
                            in
                            case result of
                                Ok a ->
                                    case f (a :: accm) of
                                        Builder h ->
                                            h ls_

                                Err _ ->
                                    ( Ok (List.reverse accm), ls )
    in
    f []


just : Builder (Maybe a) -> Builder a
just pma =
    bind pma <|
        \ma ->
            case ma of
                Just a ->
                    return a

                Nothing ->
                    fail "unexpected EOF"


ok : (x -> String) -> Builder (Result x a) -> Builder a
ok f pra =
    bind pra <|
        \ra ->
            case ra of
                Ok a ->
                    return a

                Err err ->
                    fail <| f err


string : Builder String
string =
    just head


maybeInt : Builder (Maybe Int)
maybeInt =
    head |> fmap (Maybe.andThen String.toInt)


int : Builder Int
int =
    just maybeInt


maybeFloat : Builder (Maybe Float)
maybeFloat =
    head |> fmap (Maybe.andThen String.toFloat)


float : Builder Float
float =
    just maybeFloat


stringsN : Int -> Builder (List String)
stringsN =
    let
        f accm n =
            if n <= 0 then
                return accm

            else
                bind string <| \s -> f (s :: accm) (n - 1)
    in
    f []


foldResult : (a -> b -> b) -> b -> List (Result e a) -> Result e b
foldResult cons b ls =
    case ls of
        [] ->
            Ok b

        (Ok a) :: xs ->
            foldResult cons (cons a b) xs

        (Err e) :: xs ->
            Err e


build : Builder a -> String -> Result String (List a)
build b s =
    case Parser.run P.file s of
        Ok records ->
            foldResult (::) [] <| List.map (exec b) records

        Err err ->
            Err (Parser.deadEndsToString err)
