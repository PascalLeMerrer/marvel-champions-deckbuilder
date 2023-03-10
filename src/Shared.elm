module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , Status(..)
    , init
    , subscriptions
    , update
    )

import Backend exposing (backendName, errorToString, getCardListCmd, getPackListCmd)
import Card exposing (Card)
import Http
import Json.Decode as Json
import Kinto
import Pack exposing (Pack)
import Request exposing (Request)


type alias Flags =
    Json.Value


type alias Model =
    { cards : List Card
    , cardsLoaded : Bool
    , packs : List Pack
    , packsLoaded : Bool
    , logs : List String
    , status : Status
    }


type Msg
    = BackendReturnedPackList (Result Http.Error (List Pack))
    | BackendReturnedCardList (Result Kinto.Error (Kinto.Pager Card))
    | CardListUpdated (List Card)
    | PackListUpdated (List Pack)


type Status
    = Error
    | Loading
    | Loaded
    | Initialized


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( { cards = []
      , cardsLoaded = False
      , packs = []
      , packsLoaded = False
      , logs = []
      , status = Loading
      }
    , Cmd.batch
        [ getPackListCmd BackendReturnedPackList
        , getCardListCmd BackendReturnedCardList
        ]
    )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        BackendReturnedCardList (Err kintoError) ->
            ( { model
                | logs = Kinto.errorToString kintoError :: model.logs
                , status = Error
              }
            , Cmd.none
            )

        BackendReturnedCardList (Ok pager) ->
            ( { model
                | cards = pager.objects
                , cardsLoaded = True
                , logs = ((List.length pager.objects |> String.fromInt) ++ " cartes dans la base de données") :: model.logs
                , status =
                    if model.packsLoaded then
                        Loaded

                    else
                        model.status
              }
            , Cmd.none
            )

        BackendReturnedPackList (Err httpError) ->
            ( { model
                | logs = errorToString backendName httpError :: model.logs
                , status = Error
              }
            , Cmd.none
            )

        BackendReturnedPackList (Ok packs) ->
            ( { model
                | packs = packs
                , packsLoaded = True
                , status =
                    if model.cardsLoaded then
                        Loaded

                    else
                        model.status
                , logs = ((List.length packs |> String.fromInt) ++ " packs dans la base de données") :: model.logs
              }
            , Cmd.none
            )

        CardListUpdated cards ->
            ( { model | cards = cards }, Cmd.none )

        PackListUpdated packs ->
            ( { model | packs = packs }, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
