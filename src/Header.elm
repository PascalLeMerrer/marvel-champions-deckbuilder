module Header exposing (view)

import Colors exposing (lighterGreen)
import Element as E exposing (px, rgb255)
import Element.Background as Background
import Element.Font as Font


view : Maybe String -> E.Element msg
view pageTitle =
    E.row
        [ E.width E.fill
        , E.padding 20
        , E.spacing 20
        , Background.color lighterGreen
        , Font.size 12
        ]
    <|
        [ E.link [ Font.underline ]
            { url = "/"
            , label = E.text "Accueil"
            }
        ]
            ++ (case pageTitle of
                    Just title ->
                        [ E.text ">"
                        , E.text title
                        ]

                    Nothing ->
                        []
               )
