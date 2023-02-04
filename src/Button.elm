module Button exposing (Kind(..), button, styles, withFontSize, withKind, withWidth)

import Colors exposing (charcoal, darkGreen, darkGrey, darkerGreen, darkerGrey, grey, white)
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


type Kind
    = Primary
    | Secondary


button : Kind -> String -> Int -> Int -> msg -> E.Element msg
button kind label width fontSize msg =
    Input.button
        (styles
            |> withWidth width
            |> withFontSize fontSize
            |> withKind kind
        )
        { onPress = Just msg
        , label = E.text label
        }


styles : List (E.Attr () msg)
styles =
    [ Border.rounded 7
    , E.alignRight
    , E.padding 10
    , Font.center
    ]


withKind : Kind -> List (E.Attribute msg) -> List (E.Attribute msg)
withKind kind attributes =
    attributes
        ++ (case kind of
                Primary ->
                    [ Background.color darkGreen
                    , Border.color darkerGreen
                    , Font.color white
                    , E.mouseOver
                        [ Background.color darkerGreen
                        ]
                    ]

                Secondary ->
                    [ Background.color grey
                    , Border.color darkerGrey
                    , Font.color charcoal
                    , E.mouseOver
                        [ Background.color darkGrey
                        , Font.color white
                        ]
                    ]
           )


withWidth : Int -> List (E.Attribute msg) -> List (E.Attribute msg)
withWidth width attributes =
    (E.width <| E.px width) :: attributes


withFontSize : Int -> List (E.Attribute msg) -> List (E.Attribute msg)
withFontSize size attributes =
    Font.size size :: attributes
