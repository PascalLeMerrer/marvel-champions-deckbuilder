module Pages.ImportData exposing (Model, Msg, page)

import Backend exposing (KintoData, backendName, errorToString, saveCardListCmd, savePackListCmd)
import Card exposing (Card, viewCardsTable)
import Effect exposing (Effect)
import Element as E
import Element.Font as Font
import Gen.Params.ImportData exposing (Params)
import Gen.Route as Route
import Header
import Http
import List.Extra exposing (updateIf)
import MarvelCdb exposing (loadAllCardsFromMarvelCdbCmd, loadAllPacksFromMarvelCdbCmd, marvelCDBName)
import Pack exposing (Pack)
import Page
import RemoteData exposing (RemoteData(..), WebData)
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init shared req
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


emptyModel : Model
emptyModel =
    { cards = []
    , cardCodes = []
    , marvelCdbPackIds = []
    , packs = []
    , logs = []
    }


withLogs : List String -> Model -> Model
withLogs logs model =
    { model | logs = logs }


init : Shared.Model -> Request.With Params -> ( Model, Effect Msg )
init shared req =
    case shared.status of
        Shared.Error ->
            ( emptyModel |> withLogs shared.logs
            , Effect.none
            )

        Shared.Initialized ->
            ( emptyModel |> withLogs ("initialized" :: shared.logs)
            , Effect.fromCmd <|
                Request.replaceRoute Route.Home_ req
            )

        Shared.Loading ->
            ( emptyModel |> withLogs ("loading..." :: shared.logs)
            , Effect.fromCmd <|
                Request.replaceRoute Route.Home_ req
            )

        Shared.Loaded ->
            ( { cards = shared.cards
              , cardCodes = List.map .code shared.cards
              , marvelCdbPackIds = List.map .marvelCdbId shared.packs
              , packs = shared.packs
              , logs = "loaded" :: shared.logs
              }
            , Effect.batch
                [ Effect.fromCmd <| loadAllPacksFromMarvelCdbCmd (RemoteData.fromResult >> MarvelDcbReturnedPacks)
                , Effect.fromCmd <| loadAllCardsFromMarvelCdbCmd (RemoteData.fromResult >> MarvelDcbReturnedCards)
                ]
            )



-- UPDATE


type Msg
    = MarvelDcbReturnedPacks (WebData (List Pack))
    | MarvelDcbReturnedCards (WebData (List Card))
    | BackendReturnedImportedPackList (WebData (List Pack)) --Cannot use KintoData as the batch function is not supported by elm-Kinto
    | BackendReturnedImportedCardList (WebData (List Card))
    | UserClickedUnselectedCard Card
    | UserClickedSelectedCard Card


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        BackendReturnedImportedCardList (Failure httpError) ->
            ( { model
                | logs = errorToString backendName httpError :: model.logs
              }
            , Effect.none
            )

        BackendReturnedImportedCardList (Success cards) ->
            let
                allCards =
                    model.cards ++ cards
            in
            ( { model
                | cards = allCards
                , logs = ((List.length cards |> String.fromInt) ++ " cartes importées") :: model.logs
              }
            , Effect.fromShared <| Shared.CardListUpdated allCards
            )

        BackendReturnedImportedCardList _ ->
            ( model, Effect.none )

        BackendReturnedImportedPackList (Failure httpError) ->
            ( { model
                | logs = errorToString backendName httpError :: model.logs
              }
            , Effect.none
            )

        BackendReturnedImportedPackList (Success packs) ->
            let
                allPacks =
                    model.packs ++ packs
            in
            ( { model
                | packs = allPacks
                , logs = ((List.length packs |> String.fromInt) ++ " packs importés") :: model.logs
              }
            , Effect.fromShared <| Shared.PackListUpdated allPacks
            )

        BackendReturnedImportedPackList _ ->
            ( model, Effect.none )

        MarvelDcbReturnedCards (Failure httpError) ->
            ( { model
                | logs = errorToString marvelCDBName httpError :: model.logs
              }
            , Effect.none
            )

        MarvelDcbReturnedCards (Success cards) ->
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
            , Effect.fromCmd <| saveCardListCmd newCards (RemoteData.fromResult >> BackendReturnedImportedCardList)
            )

        MarvelDcbReturnedCards _ ->
            ( model, Effect.none )

        MarvelDcbReturnedPacks (Failure httpError) ->
            ( { model
                | logs = errorToString marvelCDBName httpError :: model.logs
              }
            , Effect.none
            )

        MarvelDcbReturnedPacks (Success packs) ->
            let
                newPacks : List Pack
                newPacks =
                    -- the packs added to marvel CDB since the last importation
                    List.filter (\p -> not <| List.member p.marvelCdbId model.marvelCdbPackIds) packs
            in
            ( { model
                | logs = ((List.length newPacks |> String.fromInt) ++ " nouveaux packs reçus") :: model.logs
              }
            , Effect.fromCmd <| savePackListCmd newPacks (RemoteData.fromResult >> BackendReturnedImportedPackList)
            )

        MarvelDcbReturnedPacks _ ->
            ( model, Effect.none )

        UserClickedUnselectedCard card ->
            ( { model | cards = updateIf (\c -> c == card) (\_ -> { card | isImageVisible = True }) model.cards }
            , Effect.none
            )

        UserClickedSelectedCard card ->
            ( { model | cards = updateIf (\c -> c == card) (\_ -> { card | isImageVisible = False }) model.cards }
            , Effect.none
            )


unique : List Card -> List Card
unique cards =
    -- eliminate duplicated cards
    List.filter (\c -> c.isDuplicateOf == Nothing) cards



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Import"
    , body =
        [ E.layout
            [ E.width E.fill, E.height E.fill, E.inFront (Header.view <| Just "Import") ]
            (E.column
                [ E.paddingXY 20 80
                , E.spacing 20
                , E.width E.fill
                ]
                [ viewStatus model.logs
                , E.text "Packs"
                , viewPacks model.packs
                , E.text "Cards"
                , viewCardsTable model.cards
                    { showCount = True
                    , action = Nothing
                    , selectMsg = UserClickedUnselectedCard
                    , unselectMsg = UserClickedSelectedCard
                    , quantityChangedMsg = Nothing
                    }
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
