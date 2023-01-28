module Pages.ImportData exposing (Model, Msg, page)

import Backend exposing (backendName, errorToString, saveCardListCmd, savePackListCmd)
import Card exposing (Card)
import Effect exposing (Effect)
import Element as E exposing (rgb255)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Gen.Params.ImportData exposing (Params)
import Http
import List.Extra exposing (updateIf)
import MarvelCdb exposing (loadAllCardsFromMarvelCdbCmd, loadAllPacksFromMarvelCdbCmd, marvelCDBName)
import Pack exposing (Pack)
import Page
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init shared
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { cards : List Card
    , cardCodes : List String
    , marvelCdbPackIds : List Int
    , packs : List Pack
    , logs : List String
    }


init : Shared.Model -> ( Model, Effect Msg )
init shared =
    case shared.status of
        Shared.Error ->
            ( { cards = []
              , cardCodes = []
              , marvelCdbPackIds = []
              , packs = []
              , logs = shared.logs
              }
            , Effect.none
            )

        Shared.Loading ->
            ( { cards = []
              , cardCodes = []
              , marvelCdbPackIds = []
              , packs = []
              , logs = "loading..." :: shared.logs
              }
            , Effect.none
            )

        Shared.Loaded ->
            ( { cards = shared.cards
              , cardCodes = List.map .code shared.cards
              , marvelCdbPackIds = List.map .marvelCdbId shared.packs
              , packs = shared.packs
              , logs = "loaded" :: shared.logs
              }
            , Effect.batch
                [ Effect.fromCmd <| loadAllPacksFromMarvelCdbCmd MarvelDcbReturnedPacks
                , Effect.fromCmd <| loadAllCardsFromMarvelCdbCmd MarvelDcbReturnedCards
                ]
            )

        Shared.Initialized ->
            ( { cards = []
              , cardCodes = []
              , marvelCdbPackIds = []
              , packs = []
              , logs = "initialized" :: shared.logs
              }
            , Effect.fromShared Shared.PageReloaded
            )



-- UPDATE


type Msg
    = MarvelDcbReturnedPacks (Result Http.Error (List Pack))
    | MarvelDcbReturnedCards (Result Http.Error (List Card))
    | BackendReturnedImportedPackList (Result Http.Error (List Pack))
    | BackendReturnedImportedCardList (Result Http.Error (List Card))
    | UserClickedUnselectedCard Card
    | UserClickedSelectedCard Card


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        BackendReturnedImportedCardList (Err httpError) ->
            ( { model
                | logs = errorToString backendName httpError :: model.logs
              }
            , Effect.none
            )

        BackendReturnedImportedCardList (Ok cards) ->
            ( { model
                | cards = model.cards ++ cards
                , logs = ((List.length cards |> String.fromInt) ++ " cartes importées") :: model.logs
              }
              -- TODO update shared model
            , Effect.none
            )

        BackendReturnedImportedPackList (Err httpError) ->
            ( { model
                | logs = errorToString backendName httpError :: model.logs
              }
            , Effect.none
            )

        BackendReturnedImportedPackList (Ok packs) ->
            ( { model
                | packs = model.packs ++ packs
                , logs = ((List.length packs |> String.fromInt) ++ " packs importés") :: model.logs
              }
              -- TODO update shared model
            , Effect.none
            )

        MarvelDcbReturnedCards (Err httpError) ->
            ( { model
                | logs = errorToString marvelCDBName httpError :: model.logs
              }
            , Effect.none
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
                | logs = ((List.length newCards |> String.fromInt) ++ " nouvelles cartes reçues") :: model.logs
              }
            , Effect.fromCmd <| saveCardListCmd newCards BackendReturnedImportedCardList
            )

        MarvelDcbReturnedPacks (Err httpError) ->
            ( { model
                | logs = errorToString marvelCDBName httpError :: model.logs
              }
            , Effect.none
            )

        MarvelDcbReturnedPacks (Ok packs) ->
            let
                newPacks : List Pack
                newPacks =
                    -- the packs added to marvel CDB since the last importation
                    List.filter (\p -> not <| List.member p.marvelCdbId model.marvelCdbPackIds) packs
            in
            ( { model
                | logs = ((List.length newPacks |> String.fromInt) ++ " nouveaux packs reçus") :: model.logs
              }
            , Effect.fromCmd <| savePackListCmd newPacks BackendReturnedImportedPackList
            )

        UserClickedUnselectedCard card ->
            ( { model | cards = updateIf (\c -> c == card) (\_ -> { card | isSelected = True }) model.cards }
            , Effect.none
            )

        UserClickedSelectedCard card ->
            ( { model | cards = updateIf (\c -> c == card) (\_ -> { card | isSelected = False }) model.cards }
            , Effect.none
            )


unique : List Card -> List Card
unique cards =
    -- eliminate duplicated cards
    List.filter (\c -> c.isDuplicateOf == Nothing) cards



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
                [ viewStatus model.logs
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
