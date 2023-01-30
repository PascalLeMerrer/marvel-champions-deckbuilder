module Pages.Home_ exposing (page)

import Element as E exposing (rgb, rgb255)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
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
        [ E.layout [] <|
            E.column
                [ E.centerX
                , E.centerY
                , E.spacing 10
                ]
                [ E.link
                    linkAttributes
                    { url = "/new-pack"
                    , label = E.text "Créer un pack"
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
    [ Background.color (rgb255 31 199 170)
    , Border.color (rgb 0 0.7 0)
    , Border.rounded 7
    , E.padding 10
    , Font.size 14
    , Font.color (rgb 1 1 1)
    ]
