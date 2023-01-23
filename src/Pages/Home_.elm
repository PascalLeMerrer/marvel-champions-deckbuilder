module Pages.Home_ exposing (page)

import Element as E exposing (rgb)
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
                ]
                [ E.link
                    [ Background.color (rgb (31 / 255) (199 / 255) (170 / 255))
                    , Border.color (rgb 0 0.7 0)
                    , Border.rounded 7
                    , E.padding 10
                    , Font.size 14
                    , Font.color (rgb 1 1 1)
                    ]
                    { url = "/import-data"
                    , label = E.text "Importer"
                    }
                ]
        ]
    }
