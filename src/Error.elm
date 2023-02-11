module Error exposing (viewError)

import Colors exposing (charcoal, darkerRed, lighterRed)
import Element as E exposing (fill, padding, width)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font


viewError : { a | error : Maybe String } -> E.Element msg
viewError model =
    case model.error of
        Just errorMessage ->
            E.el
                [ Background.color lighterRed
                , Border.color darkerRed
                , Border.solid
                , Border.width 2
                , Font.color darkerRed
                , padding 10
                , rounded 7
                , width fill
                ]
            <|
                E.paragraph [] [ E.text errorMessage ]

        Nothing ->
            E.none
