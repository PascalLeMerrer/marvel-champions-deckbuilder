module Pages.Decks exposing (Model, Msg, page)

import Backend exposing (getDeckListCmd)
import Colors exposing (black, charcoal, darkerGreen, grey, lightGrey, white)
import Deck exposing (Deck)
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Gen.Params.Decks exposing (Params)
import Header
import Kinto exposing (errorToString)
import Page
import Request
import Shared
import Table
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { decks : List Deck
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { decks = []
      , error = Nothing
      }
    , getDeckListCmd BackendReturnedDeckList
    )



-- UPDATE


type Msg
    = BackendReturnedDeckList (Result Kinto.Error (Kinto.Pager Deck))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BackendReturnedDeckList (Ok decks) ->
            ( { model | decks = decks.objects }
            , Cmd.none
            )

        BackendReturnedDeckList (Err kintoError) ->
            ( { model | error = Just (errorToString kintoError) }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Import"
    , body =
        [ E.layout
            [ E.width E.fill
            , E.height E.fill
            , E.inFront (Header.view <| Just "Mes decks")
            ]
          <|
            E.el
                [ E.paddingXY 20 80
                ]
                (viewDecks model)
        ]
    }


viewDecks : Model -> E.Element msg
viewDecks model =
    E.indexedTable
        [ Border.color charcoal
        , Font.size 11
        , Font.color white
        ]
        { data = model.decks
        , columns =
            [ { header = E.el Table.headerAttributes (E.text "Titre")
              , width = E.fill
              , view = viewDeck
              }
            ]
        }


rowAttributes : Int -> List (E.Attribute msg)
rowAttributes index =
    [ Background.color <|
        if modBy 2 index == 0 then
            grey

        else
            lightGrey
    , E.mouseOver
        [ Font.color darkerGreen
        ]
    , E.alignTop
    , E.padding 5
    , E.pointer
    , E.height E.fill
    , Font.color black
    ]


viewDeck : Int -> Deck -> E.Element msg
viewDeck index deck =
    E.el (rowAttributes index) <|
        E.link []
            { label = E.text deck.title
            , url = "/deck/" ++ deck.id
            }
