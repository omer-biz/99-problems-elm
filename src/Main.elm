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


main : Html.Html msg
main =
    [ "x", "a", "m", "a", "x" ]
        |> isPalendrome
        |> Debug.toString
        |> Html.text
