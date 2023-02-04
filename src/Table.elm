module Table exposing (headerAttributes)

import Colors exposing (charcoal, darkGreen)
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


headerAttributes : List (E.Attribute msg)
headerAttributes =
    [ Background.color darkGreen
    , E.alignTop
    , Font.bold
    , Font.size 13
    , E.paddingXY 5 10
    , Border.widthEach
        { bottom = 1
        , left = 0
        , right = 0
        , top = 0
        }
    , Border.color charcoal
    ]
