module Pages.Home_ exposing (page)

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
                    linkAttributes
                    { url = "/new-deck"
                    , label = E.text "Créer un deck"
                    }
                , E.link
                    linkAttributes
                    { url = "/import-data"
                    , label = E.text "Importer"
                    }
                ]
        ]
    }


linkAttributes : List (E.Attr () msg)
linkAttributes =
    [ Background.color darkGreen
    , Border.color darkerGreen
    , E.mouseOver
        [ Background.color darkerGreen
        ]
    , Border.rounded 7
    , E.padding 10
    , E.width <| px 150
    , Font.center
    , Font.size 16
    , Font.color white
    ]
