module Backend exposing (KintoData, backendName, createDeckCmd, createPack, errorToString, getCardListCmd, getDeckCmd, getDeckListCmd, getPackListCmd, saveCardListCmd, saveDeckCmd, savePackListCmd)

{- Exchanges with Kinto -}

import Card exposing (Card, cardListDecoder, decoder, encodeNewCard)
import Deck exposing (Deck)
import Http
import Json.Decode exposing (Decoder)
import Json.Encode as Encode
import Kinto
import List.Split exposing (chunksOfLeft)
import Pack exposing (Pack, encodeNewPack, packDecoder, packListDecoder)
import RemoteData exposing (RemoteData)


client : Kinto.Client
client =
    Kinto.client
        "http://localhost:8888/v1/"
        (Kinto.Basic "app" "azxd4de3ffg5Gfd68jUUk90l")


bucketName =
    "deckbuilder"


baseUrl : String
baseUrl =
    "http://localhost:8888/v1"


collectionsUrl : String
collectionsUrl =
    baseUrl ++ "/buckets/deckbuilder/collections"


authHeader : Http.Header
authHeader =
    -- TODO encode auth token from plain text value
    Http.header "Authorization" "Basic YXBwOmF6eGQ0ZGUzZmZnNUdmZDY4alVVazkwbA=="


timeOutMiliseconds : Maybe Float
timeOutMiliseconds =
    Just 90000


type alias KintoData a =
    RemoteData Kinto.Error a



-- PACKS --


packCollection =
    "packs"


recordPack : Kinto.Resource Pack
recordPack =
    Kinto.recordResource bucketName packCollection Pack.packDecoder



-- TODO use


createPack : Pack -> (Result Kinto.Error Pack -> msg) -> Cmd msg
createPack pack msg =
    let
        data =
            Pack.encodeNewPack pack
    in
    client
        |> Kinto.create recordPack data msg
        |> Kinto.send


getPackListCmd : (Result Http.Error (List Pack) -> msg) -> Cmd msg
getPackListCmd msg =
    Http.request
        { method = "GET"
        , headers = [ authHeader ]
        , url = collectionsUrl ++ "/packs/records"
        , body = Http.emptyBody
        , expect = Http.expectJson msg packListDecoder
        , timeout = timeOutMiliseconds
        , tracker = Nothing
        }


savePackListCmd : List Pack -> (Result Http.Error (List Pack) -> msg) -> Cmd msg
savePackListCmd packs msg =
    if List.isEmpty packs then
        Cmd.none

    else
        packs
            |> chunksOfLeft 25
            |> List.map (savePackSubListCmd msg)
            |> Cmd.batch


savePackSubListCmd : (Result Http.Error (List Pack) -> msg) -> List Pack -> Cmd msg
savePackSubListCmd msg packs =
    Http.request
        { method = "POST"
        , headers = [ authHeader ]
        , url = baseUrl ++ "/batch"
        , body = Http.jsonBody (encodePackListCreationPayload packs)
        , expect = Http.expectJson msg packListCreationDecoder
        , timeout = timeOutMiliseconds
        , tracker = Nothing
        }


packListCreationDecoder : Decoder (List Pack)
packListCreationDecoder =
    Json.Decode.field "responses" <|
        Json.Decode.list <|
            Json.Decode.field "body" <|
                Json.Decode.field "data" packDecoder


encodePackCreationPayload : Pack -> Encode.Value
encodePackCreationPayload pack =
    Encode.object <|
        [ ( "data", encodeNewPack pack )
        ]


encodePackListCreationPayload : List Pack -> Encode.Value
encodePackListCreationPayload packs =
    Encode.object <|
        [ ( "defaults"
          , Encode.object
                [ ( "method", Encode.string "POST" )
                , ( "path", Encode.string "/buckets/deckbuilder/collections/packs/records" )
                ]
          )
        , ( "requests"
          , Encode.list encodePackCreationBody packs
          )
        ]


encodePackCreationBody : Pack -> Encode.Value
encodePackCreationBody pack =
    Encode.object
        [ ( "body"
          , encodePackCreationPayload pack
          )
        ]



-- CARDS --


cardCollectionName : String
cardCollectionName =
    "cards"


cardRecord : Kinto.Resource Card
cardRecord =
    Kinto.recordResource bucketName cardCollectionName Card.decoder


getCardListCmd : (Result Kinto.Error (Kinto.Pager Card) -> msg) -> Cmd msg
getCardListCmd msg =
    client
        |> Kinto.getList cardRecord msg
        |> Kinto.sort [ "title", "description" ]
        |> Kinto.send


saveCardListCmd : List Card -> (Result Http.Error (List Card) -> msg) -> Cmd msg
saveCardListCmd cards msg =
    if List.isEmpty cards then
        Cmd.none

    else
        cards
            |> chunksOfLeft 25
            |> List.map (saveCardSubListCmd msg)
            |> Cmd.batch


saveCardSubListCmd : (Result Http.Error (List Card) -> msg) -> List Card -> Cmd msg
saveCardSubListCmd msg cards =
    Http.request
        { method = "POST"
        , headers = [ authHeader ]
        , url = baseUrl ++ "/batch"
        , body = Http.jsonBody (encodeCardListCreationPayload cards)
        , expect = Http.expectJson msg cardListCreationDecoder
        , timeout = timeOutMiliseconds
        , tracker = Nothing
        }


cardListCreationDecoder : Decoder (List Card)
cardListCreationDecoder =
    Json.Decode.field "responses" <|
        Json.Decode.list <|
            Json.Decode.field "body" <|
                Json.Decode.field "data" decoder


encodeCardCreationPayload : Card -> Encode.Value
encodeCardCreationPayload card =
    Encode.object <|
        [ ( "data", encodeNewCard card )
        ]


encodeCardListCreationPayload : List Card -> Encode.Value
encodeCardListCreationPayload cards =
    Encode.object <|
        [ ( "defaults"
          , Encode.object
                [ ( "method", Encode.string "POST" )
                , ( "path", Encode.string "/buckets/deckbuilder/collections/cards/records" )
                ]
          )
        , ( "requests"
          , Encode.list encodeCardCreationBody cards
          )
        ]


encodeCardCreationBody : Card -> Encode.Value
encodeCardCreationBody card =
    Encode.object
        [ ( "body"
          , encodeCardCreationPayload card
          )
        ]


backendName : String
backendName =
    "The backend"


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



-- DECKS --


deckCollectionName : String
deckCollectionName =
    "decks"


deckRecord : Kinto.Resource Deck
deckRecord =
    Kinto.recordResource bucketName deckCollectionName Deck.decoder


createDeckCmd : Deck -> (Result Kinto.Error Deck -> msg) -> Cmd msg
createDeckCmd deck msg =
    let
        data =
            Deck.encodeDeckCreationPayload deck
    in
    client
        |> Kinto.create deckRecord data msg
        |> Kinto.send


saveDeckCmd : Deck -> (Result Kinto.Error Deck -> msg) -> Cmd msg
saveDeckCmd deck msg =
    let
        data =
            Deck.encode deck
    in
    client
        |> Kinto.replace deckRecord deck.id data msg
        |> Kinto.send


getDeckListCmd : (Result Kinto.Error (Kinto.Pager Deck) -> msg) -> Cmd msg
getDeckListCmd msg =
    client
        |> Kinto.getList deckRecord msg
        |> Kinto.sort [ "title" ]
        |> Kinto.send


getDeckCmd : String -> (Result Kinto.Error Deck -> msg) -> Cmd msg
getDeckCmd deckId msg =
    client
        |> Kinto.get deckRecord deckId msg
        |> Kinto.send
