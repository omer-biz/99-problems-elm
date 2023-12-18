module Main exposing (main)

import Html


last : List a -> Maybe a
last list =
    case list of
        [] ->
            Nothing

        [ x ] ->
            Just x

        _ :: t ->
            last t



-- Part One End


lastTwo : List a -> Maybe ( a, a )
lastTwo list =
    case list of
        [] ->
            Nothing

        [ _ ] ->
            Nothing

        [ x, y ] ->
            Just ( x, y )

        _ :: t ->
            lastTwo t



-- Part Two End


at : Int -> List a -> Maybe a
at index list =
    case ( index, list ) of
        ( _, [] ) ->
            Nothing

        ( 1, el :: _ ) ->
            Just el

        ( n, _ :: rest ) ->
            at (n - 1) rest



-- Part Three End


length : List a -> Int
length list =
    -- I implemented tco without knowing what it is :)
    let
        len index lt =
            case ( index, lt ) of
                ( 0, [] ) ->
                    0

                ( n, [] ) ->
                    n

                ( n, _ :: rest ) ->
                    len (n - 1) rest
    in
    len 0 list * -1


lengthTail : List a -> Int
lengthTail list =
    -- with proper tco
    let
        len index lt =
            case ( index, lt ) of
                ( n, [] ) ->
                    n

                ( n, _ :: rest ) ->
                    len (n + 1) rest
    in
    len 0 list


lengthTwo : List a -> number
lengthTwo list =
    -- found from mr. tsoding
    case list of
        [] ->
            0

        _ :: rest ->
            1 + lengthTwo rest



-- Part Four End


reverse : List a -> List a
reverse list =
    let
        rev l1 l2 =
            case ( l1, l2 ) of
                ( [], l ) ->
                    l

                ( el :: rest, l ) ->
                    rev rest (el :: l)
    in
    rev list []



-- Part Five End


isPalendrome : List a -> Bool
isPalendrome list =
    list == reverse list



-- Part Six End


type Node a
    = One a
    | Many (List (Node a))


flatten : List a -> List (Node a) -> List a
flatten return nodes =
    case nodes of
        [] ->
            return

        f :: r ->
            case f of
                One val ->
                    flatten (val :: return) r

                Many ns ->
                    flatten return (r ++ ns)


flattenAns : List a -> List (Node a) -> List a
flattenAns return nodes =
    case nodes of
        [] ->
            return

        f :: r ->
            case f of
                One val ->
                    flattenAns (val :: return) r

                Many ns ->
                    flattenAns (flattenAns return ns) r



-- Part Seven End


compress : List a -> List a
compress list =
    let
        aux acc l =
            case l of
                [] ->
                    acc

                first :: rest ->
                    if List.head acc == Just first then
                        aux acc rest

                    else
                        aux (first :: acc) rest
    in
    aux [] list


compressAns : List a -> List a
compressAns list =
    case list of
        a :: ((b :: _) as rest) ->
            if a == b then
                compress rest

            else
                a :: compress rest

        smaller ->
            smaller



-- Part Eight End


pack : List a -> List (List a)
pack list =
    let
        aux current acc l =
            case l of
                [] ->
                    []

                [ x ] ->
                    (x :: current) :: acc

                a :: ((b :: _) as rest) ->
                    if a == b then
                        aux (a :: current) acc rest

                    else
                        aux [] ((a :: current) :: acc) rest
    in
    List.reverse <| aux [] [] list



-- Part Nine End


encode : List a -> List ( Int, Maybe a )
encode list =
    list
        |> pack
        |> List.map (\l -> ( List.length l, List.head l ))


encodeAns : List a -> List ( number, a )
encodeAns list =
    let
        aux count acc l =
            case l of
                [] ->
                    []

                [ x ] ->
                    ( count + 1, x ) :: acc

                a :: ((b :: _) as rest) ->
                    if a == b then
                        aux (count + 1) acc rest

                    else
                        aux 0 (( count + 1, a ) :: acc) rest
    in
    List.reverse <| aux 0 [] list



-- Part Ten End


type Rle a
    = Single a
    | Multiple ( Int, a )


encodeMod : List b -> List (Rle b)
encodeMod list =
    let
        changeToRle ( n, item ) =
            if n == 1 then
                Single item

            else
                Multiple ( n, item )
    in
    list
        |> encodeAns
        |> List.map changeToRle



-- Part Eleven End


main : Html.Html msg
main =
    [ "a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e" ]
        |> encodeMod
        |> Debug.toString
        |> Html.text
