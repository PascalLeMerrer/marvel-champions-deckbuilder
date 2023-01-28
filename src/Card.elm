module Card exposing (Card, cardDecoder, cardListDecoder, encodeCard, encodeNewCard)

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode


type alias Card =
    { id : String
    , code : String
    , imagesrc : Maybe String
    , isSelected : Bool
    , isDuplicateOf : Maybe String
    , kind : String -- TODO A changer en enum
    , name : String
    , faction : String -- TODO A changer en enum
    }



{- Encoders/ decoders for interactions with the backend -}


cardDecoder : Decoder Card
cardDecoder =
    Json.Decode.succeed Card
        |> required "id" Json.Decode.string
        |> required "code" Json.Decode.string
        |> optional "imagesrc" (Json.Decode.map Just Json.Decode.string) Nothing
        |> hardcoded False
        |> optional "duplicate_of_code" (Json.Decode.map Just Json.Decode.string) Nothing
        |> required "kind" Json.Decode.string
        |> required "name" Json.Decode.string
        |> required "faction" Json.Decode.string


cardListDecoder : Decoder (List Card)
cardListDecoder =
    Json.Decode.field "data" <|
        Json.Decode.list cardDecoder


encodeCard : Card -> Encode.Value
encodeCard card =
    Encode.object <|
        [ ( "id", Encode.string card.id )
        , ( "code", Encode.string card.code )
        , ( "kind", Encode.string card.kind )
        , ( "name", Encode.string card.name )
        , ( "faction", Encode.string card.faction )
        ]


encodeNewCard : Card -> Encode.Value
encodeNewCard card =
    -- when creating a new card, we cannot pass an ID; the backend will generate it
    Encode.object <|
        [ ( "code", Encode.string card.code )
        , ( "kind", Encode.string card.kind )
        , ( "imagesrc"
          , Encode.string
                (case card.imagesrc of
                    Just url ->
                        url

                    Nothing ->
                        ""
                )
          )
        , ( "name", Encode.string card.name )
        , ( "faction", Encode.string card.faction )
        ]
