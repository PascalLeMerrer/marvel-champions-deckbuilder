module Pages.Decks exposing (Model, Msg, page)

import Backend exposing (KintoData, getDeckListCmd)
import Colors exposing (black, charcoal, darkerGreen, grey, lightGrey, white)
import Deck exposing (Deck)
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Error exposing (viewError)
import Faction
import Gen.Params.Decks exposing (Params)
import Header
import Kinto exposing (errorToString)
import Page
import RemoteData exposing (RemoteData(..))
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
    , isLoaded : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { decks = []
      , error = Nothing
      , isLoaded = False
      }
    , getDeckListCmd (RemoteData.fromResult >> BackendReturnedDeckList)
    )



-- UPDATE


type Msg
    = BackendReturnedDeckList (KintoData (Kinto.Pager Deck))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BackendReturnedDeckList (Success decks) ->
            ( { model
                | decks = decks.objects
                , isLoaded = True
              }
            , Cmd.none
            )

        BackendReturnedDeckList (Failure kintoError) ->
            ( { model | error = Just (errorToString kintoError) }
            , Cmd.none
            )

        BackendReturnedDeckList _ ->
            ( model
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
            E.column
                [ E.paddingXY 20 80
                , E.spacing 20
                ]
                [ viewError model
                , viewBody model
                ]
        ]
    }


viewBody : Model -> E.Element msg
viewBody model =
    if model.isLoaded then
        if List.isEmpty model.decks then
            E.link
                [ E.mouseOver
                    [ Font.color darkerGreen
                    ]
                ]
                { url = "/new-deck"
                , label = E.text "Vous n'avez aucun deck. Cliquez ici pour cr??er votre premier deck."
                }

        else
            viewDecks model

    else if model.error == Nothing then
        E.text "Chargement en cours..."

    else
        E.none


viewDecks : Model -> E.Element msg
viewDecks model =
    E.indexedTable
        [ Border.color charcoal
        , Font.size 11
        , Font.color white
        ]
        { data = model.decks |> List.sortBy (.hero >> .name)
        , columns =
            [ { header = E.el Table.headerAttributes (E.text "H??ros")
              , width = E.fill
              , view = viewHero
              }
            , { header = E.el Table.headerAttributes (E.text "Titre")
              , width = E.fill
              , view = viewTitle
              }
            , { header = E.el Table.headerAttributes (E.text "Affinit??s")
              , width = E.fill
              , view = viewAffinities
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


viewTitle : Int -> Deck -> E.Element msg
viewTitle index deck =
    E.el (rowAttributes index) <|
        E.link []
            { label = E.text deck.title
            , url = "/deck/" ++ deck.id
            }


viewHero : Int -> Deck -> E.Element msg
viewHero index deck =
    E.el (rowAttributes index) <|
        E.link []
            { label = E.text deck.hero.name
            , url = "/deck/" ++ deck.id
            }


viewAffinities : Int -> Deck -> E.Element msg
viewAffinities index deck =
    E.el (rowAttributes index) <|
        E.link []
            { label =
                E.text
                    (deck.affinities
                        |> List.map Faction.toString
                        |> String.join ", "
                    )
            , url = "/deck/" ++ deck.id
            }
