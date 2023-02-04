module Pages.Home_ exposing (page)

import Button exposing (withFontSize, withKind, withWidth)
import Colors exposing (darkGreen, darkerGreen, white)
import Element as E exposing (px)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Header
import Page exposing (Page)
import Request exposing (Request)
import Shared
import View exposing (View)


page : Shared.Model -> Request -> Page
page shared req =
    Page.static
        { view = view req
        }


view : Request -> View msg
view req =
    { title = "Homepage"
    , body =
        [ E.layout [ E.width E.fill, E.height E.fill, E.inFront <| Header.view Nothing ] <|
            E.column
                [ E.centerX
                , E.centerY
                , E.spacing 20
                ]
                [ E.link
                    (Button.styles
                        |> withKind Button.Primary
                        |> withWidth 150
                        |> withFontSize 16
                    )
                    { url = "/new-deck"
                    , label = E.text "Créer un deck"
                    }
                , E.link
                    (Button.styles
                        |> withKind Button.Secondary
                        |> withWidth 150
                        |> withFontSize 16
                    )
                    { url = "/import-data"
                    , label = E.text "Importer"
                    }
                ]
        ]
    }
