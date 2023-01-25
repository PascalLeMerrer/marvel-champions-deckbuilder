module Pages.ImportData exposing (Model, Msg, page)

import Backend exposing (getCardListCmd, getPackListCmd, saveCardListCmd, savePackListCmd)
import Card exposing (Card, newCardListDecoder)
import Element as E exposing (rgb255)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Gen.Params.ImportData exposing (Params)
import Http
import List.Extra exposing (updateIf)
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



-- Constants


cardsUrl =
    "https://fr.marvelcdb.com/api/public/cards/"


packsUrl =
    "https://fr.marvelcdb.com/api/public/packs/"


backendName =
    "The backend"


marvelCDBName =
    "The Marvel CDB server"



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
    , cardCodes : List String
    , marvel_cdb_pack_ids : List Int
    , packs : List Pack
    , status : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { cards = []
      , cardCodes = []
      , marvel_cdb_pack_ids = []
      , packs = []
      , status = [ "Chargement de la liste des cartes" ]
      }
    , getPackListCmd BackendReturnedPackList
    )



{-
   init
       -> getPackListCmd
       -> BackendReturnedPackList
       -> packImportationCmd
       -> MarvelDcbReturnedPacks
       -> savePackListCmd
       -> BackendReturnedImportedPackList
       -> getCardListCmd
       -> BackendReturnedCardList
       -> cardImportationCmd
       -> MarvelDcbReturnedCards
       -> saveCardListCmd
       -> BackendReturnedImportedCardList


-}
-- UPDATE


type Msg
    = MarvelDcbReturnedPacks (Result Http.Error (List Pack))
    | MarvelDcbReturnedCards (Result Http.Error (List Card))
    | BackendReturnedImportedPackList (Result Http.Error (List Pack))
    | BackendReturnedPackList (Result Http.Error (List Pack))
    | BackendReturnedCardList (Result Http.Error (List Card))
    | BackendReturnedImportedCardList (Result Http.Error (List Card))
    | UserClickedUnselectedCard Card
    | UserClickedSelectedCard Card


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BackendReturnedImportedCardList (Err httpError) ->
            ( { model
                | status = errorToString backendName httpError :: model.status
              }
            , Cmd.none
            )

        BackendReturnedImportedCardList (Ok cards) ->
            ( { model
                | cards = model.cards ++ cards
                , status = ((List.length cards |> String.fromInt) ++ " cartes importées") :: model.status
              }
            , Cmd.none
            )

        BackendReturnedCardList (Ok cards) ->
            ( { model
                | cards = cards
                , cardCodes = cards |> List.map .code
                , status = ((List.length cards |> String.fromInt) ++ " cartes dans la base de données") :: model.status
              }
            , cardImportationCmd
            )

        BackendReturnedCardList (Err httpError) ->
            ( { model
                | status = errorToString backendName httpError :: model.status
              }
            , Cmd.none
            )

        BackendReturnedImportedPackList (Err httpError) ->
            ( { model
                | status = errorToString backendName httpError :: model.status
              }
            , Cmd.none
            )

        BackendReturnedImportedPackList (Ok packs) ->
            ( { model
                | packs = model.packs ++ packs
                , status = ((List.length packs |> String.fromInt) ++ " packs importés") :: model.status
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
                | packs = packs
                , marvel_cdb_pack_ids = packs |> List.map .marvel_cdb_id
                , status = ((List.length packs |> String.fromInt) ++ " packs dans la base de données") :: model.status
              }
            , packImportationCmd
            )

        MarvelDcbReturnedCards (Err httpError) ->
            ( { model
                | status = errorToString marvelCDBName httpError :: model.status
              }
            , Cmd.none
            )

        MarvelDcbReturnedCards (Ok cards) ->
            let
                newCards : List Card
                newCards =
                    -- the cards added to marvel CDB since the last importation
                    cards
                        |> unique
                        |> List.filter (\card -> not <| List.member card.code model.cardCodes)
            in
            ( { model
                | status = ((List.length newCards |> String.fromInt) ++ " nouvelles cartes reçues") :: model.status
              }
            , saveCardListCmd newCards BackendReturnedImportedCardList
            )

        MarvelDcbReturnedPacks (Err httpError) ->
            ( { model
                | status = errorToString marvelCDBName httpError :: model.status
              }
            , Cmd.none
            )

        MarvelDcbReturnedPacks (Ok packs) ->
            let
                newPacks : List Pack
                newPacks =
                    -- the packs added to marvel CDB since the last importation
                    List.filter (\p -> not <| List.member p.marvel_cdb_id model.marvel_cdb_pack_ids) packs
            in
            ( { model
                | status = ((List.length newPacks |> String.fromInt) ++ " nouveaux packs reçus") :: model.status
              }
            , Cmd.batch
                [ savePackListCmd newPacks BackendReturnedImportedPackList
                , getCardListCmd BackendReturnedCardList
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


unique : List Card -> List Card
unique cards =
    -- eliminate duplicated cards
    List.filter (\c -> c.isDuplicateOf == Nothing) cards


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
