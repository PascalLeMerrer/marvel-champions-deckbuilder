module Pages.ImportData exposing (Model, Msg, page)

import Backend exposing (saveCardListCmd, savePackListCmd)
import Card exposing (Card, newCardListDecoder)
import Element as E exposing (rgb, rgb255)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Gen.Params.ImportData exposing (Params)
import Http
import List.Extra exposing (updateIf)
import NaturalOrdering exposing (compareOn)
import Pack exposing (Pack, newPackListDecoder)
import Page
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Contants


cardsUrl =
    "https://fr.marvelcdb.com/api/public/cards/"


packsUrl =
    "https://fr.marvelcdb.com/api/public/packs/"


backendName =
    "The backend"


marvelDCBName =
    "The Marvel CDB"



---


packImportationCmd : Cmd Msg
packImportationCmd =
    Http.get
        { url = packsUrl
        , expect = Http.expectJson MarvelDcbReturnedPacks newPackListDecoder
        }


cardImportationCmd : Cmd Msg
cardImportationCmd =
    Http.get
        { url = cardsUrl
        , expect = Http.expectJson MarvelDcbReturnedCards newCardListDecoder
        }



-- INIT


type alias Model =
    { cards : List Card
    , packs : List Pack
    , status : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { cards = []
      , packs = []
      , status = [ "Importing packs..." ]
      }
    , packImportationCmd
    )



-- UPDATE


type Msg
    = MarvelDcbReturnedPacks (Result Http.Error (List Pack))
    | MarvelDcbReturnedCards (Result Http.Error (List Card))
    | BackendReturnedPackList (Result Http.Error (List Pack))
    | BackendReturnedCardList (Result Http.Error (List Card))
    | UserClickedUnselectedCard Card
    | UserClickedSelectedCard Card


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BackendReturnedCardList (Err httpError) ->
            ( { model
                | status = errorToString backendName httpError :: model.status
              }
            , Cmd.none
            )

        BackendReturnedCardList (Ok cards) ->
            ( { model
                | cards = model.cards ++ cards
                , status = ((String.fromInt <| List.length cards) ++ " cards imported") :: model.status
              }
            , Cmd.none
            )

        BackendReturnedPackList (Err httpError) ->
            ( { model
                | status = errorToString backendName httpError :: model.status
              }
            , Cmd.none
            )

        BackendReturnedPackList (Ok packs) ->
            ( { model
                | packs = model.packs ++ packs
                , status = ((String.fromInt <| List.length packs) ++ " packs imported") :: model.status
              }
            , Cmd.none
            )

        MarvelDcbReturnedCards (Err httpError) ->
            ( { model
                | status = errorToString marvelDCBName httpError :: model.status
              }
            , Cmd.none
            )

        MarvelDcbReturnedCards (Ok cards) ->
            let
                nonDuplicateCards =
                    List.filter (\c -> c.isDuplicateOf == Nothing) cards
            in
            ( { model
                | status = ((String.fromInt <| List.length cards) ++ " cards received") :: model.status
              }
            , saveCardListCmd nonDuplicateCards BackendReturnedCardList
            )

        MarvelDcbReturnedPacks (Err httpError) ->
            ( { model
                | status = errorToString marvelDCBName httpError :: model.status
              }
            , Cmd.none
            )

        MarvelDcbReturnedPacks (Ok packs) ->
            ( { model
                | status = ((String.fromInt <| List.length packs) ++ " packs received") :: model.status
              }
            , Cmd.batch
                [ cardImportationCmd
                , savePackListCmd packs BackendReturnedPackList
                ]
            )

        UserClickedUnselectedCard card ->
            ( { model | cards = updateIf (\c -> c == card) (\_ -> { card | isSelected = True }) model.cards }
            , Cmd.none
            )

        UserClickedSelectedCard card ->
            ( { model | cards = updateIf (\c -> c == card) (\_ -> { card | isSelected = False }) model.cards }
            , Cmd.none
            )


errorToString : String -> Http.Error -> String
errorToString serverName error =
    case error of
        Http.BadUrl url ->
            serverName ++ " server URL is incorrect: " ++ url

        Http.Timeout ->
            serverName ++ " server did not respond"

        Http.NetworkError ->
            "Please verify your internet connection."

        Http.BadStatus code ->
            serverName ++ " server returned an error " ++ String.fromInt code

        Http.BadBody body ->
            serverName ++ " server returned an unexpected content " ++ body



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
            []
            (E.column
                [ E.padding 20
                , E.spacing 20
                ]
                [ viewStatus model.status
                , E.text "Packs"
                , viewPacks model.packs
                , E.text "Cards"
                , viewCardsTable model.cards
                ]
            )
        ]
    }


viewStatus : List String -> E.Element Msg
viewStatus statuses =
    E.column
        [ Font.size 11
        ]
    <|
        (statuses |> List.reverse |> List.map E.text)


viewPacks : List Pack -> E.Element Msg
viewPacks packs =
    packs
        |> List.sortBy .name
        |> List.map viewPack
        |> E.column [ Font.size 10 ]


viewPack : Pack -> E.Element Msg
viewPack pack =
    E.row
        []
        [ E.text pack.name
        , case pack.status of
            Pack.Received ->
                E.text "..."

            Pack.Saved ->
                E.text " ✓"
        ]


viewCardsTable : List Card -> E.Element Msg
viewCardsTable cards =
    let
        sortedCards : List Card
        sortedCards =
            --List.sortWith (compareOn (.name >> removeLeadingQuote)) cards
            List.sortBy .code cards
    in
    E.indexedTable
        [ Border.color (rgb255 3 95 22)
        , Border.solid
        , Border.width 1
        , Font.size 11
        ]
        { data = sortedCards
        , columns =
            [ { header = E.el headerAttributes (E.text "Carte")
              , width = E.fill
              , view = viewCard
              }
            , { header = E.el headerAttributes (E.text "Code")
              , width = E.fill
              , view =
                    \index card ->
                        E.el (rowAttributes index card) (E.text card.code)
              }
            , { header = E.el headerAttributes (E.text "Name")
              , width = E.fill
              , view =
                    \index card ->
                        E.el (rowAttributes index card) (E.text card.name)
              }
            , { header = E.el headerAttributes (E.text "Kind")
              , width = E.fill
              , view =
                    \index card ->
                        E.el (rowAttributes index card) (E.text card.kind)
              }
            ]
        }


rowAttributes : Int -> Card -> List (E.Attribute Msg)
rowAttributes index card =
    [ Background.color <|
        if modBy 2 index == 0 then
            rgb255 205 203 203

        else
            rgb255 246 244 244
    , if card.isSelected then
        onClick (UserClickedSelectedCard card)

      else
        onClick (UserClickedUnselectedCard card)
    , E.alignTop
    , E.padding 5
    , E.pointer
    ]


headerAttributes : List (E.Attribute Msg)
headerAttributes =
    [ Background.color <|
        rgb255 42 245 240
    , E.alignTop
    , Font.bold
    , E.padding 5
    , Border.widthEach
        { bottom = 1
        , left = 0
        , right = 0
        , top = 0
        }
    , Border.color <| rgb255 62 62 62
    ]


viewCard : Int -> Card -> E.Element Msg
viewCard index card =
    let
        attributes =
            rowAttributes index card
    in
    case ( card.imagesrc, card.isSelected ) of
        ( _, False ) ->
            E.el (Font.italic :: attributes) <| E.text "Cliquer pour afficher l'image"

        ( Nothing, True ) ->
            E.el (Font.italic :: attributes) <| E.text "Cliquer pour afficher l'image"

        ( Just path, True ) ->
            E.image
                attributes
                { src = "https://fr.marvelcdb.com" ++ path, description = card.name }


removeLeadingQuote : String -> String
removeLeadingQuote string =
    if String.startsWith "\"" string then
        String.dropLeft 1 string

    else
        string
