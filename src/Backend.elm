module Backend exposing (saveCardListCmd, savePackListCmd)

{- Exchanges with Kinto -}

import Card exposing (Card, cardDecoder, encodeNewCard)
import Http
import Json.Decode exposing (Decoder)
import Json.Encode as Encode
import List.Split exposing (chunksOfLeft)
import Pack exposing (Pack, encodeNewPack, packDecoder)


baseUrl =
    "http://localhost:8888/v1/"


authHeader =
    -- TODO encode auth token from plain text value
    Http.header "Authorization" "Basic YXBwOmF6eGQ0ZGUzZmZnNUdmZDY4alVVazkwbA=="


timeOutMiliseconds : Maybe Float
timeOutMiliseconds =
    Just 90000



-- PACKS --
--savePackCmd : Pack -> (Result Http.Error (List Pack) -> msg) -> Cmd msg
--savePackCmd pack msg =
--    Http.request
--        { method = "POST"
--        , headers = [ authHeader ]
--        , url = baseUrl ++ "buckets/deckbuilder/collections/packs/records"
--        , body = Http.jsonBody (encodePackCreationPayload pack)
--        , expect = Http.expectJson msg packListCreationDecoder
--        , timeout = timeOutMiliseconds
--        , tracker = Nothing
--        }


savePackListCmd : List Pack -> (Result Http.Error (List Pack) -> msg) -> Cmd msg
savePackListCmd packs msg =
    packs
        |> chunksOfLeft 25
        |> List.map (savePackSubListCmd msg)
        |> Cmd.batch


savePackSubListCmd : (Result Http.Error (List Pack) -> msg) -> List Pack -> Cmd msg
savePackSubListCmd msg packs =
    Http.request
        { method = "POST"
        , headers = [ authHeader ]
        , url = baseUrl ++ "batch"
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


saveCardListCmd : List Card -> (Result Http.Error (List Card) -> msg) -> Cmd msg
saveCardListCmd cards msg =
    cards
        |> chunksOfLeft 25
        |> List.map (saveCardSubListCmd msg)
        |> Cmd.batch


saveCardSubListCmd : (Result Http.Error (List Card) -> msg) -> List Card -> Cmd msg
saveCardSubListCmd msg cards =
    Http.request
        { method = "POST"
        , headers = [ authHeader ]
        , url = baseUrl ++ "batch"
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
                Json.Decode.field "data" cardDecoder


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
