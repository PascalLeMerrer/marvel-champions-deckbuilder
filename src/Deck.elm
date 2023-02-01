module Deck exposing (..)

import Card exposing (Card)
import Faction exposing (Faction(..))
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode


type alias Deck =
    { id : String
    , affinities : List Faction -- TODO prevent it to include hero
    , cards : List Card -- affinities + basic cards
    , hero : Card
    , heroCards : List Card -- cards from the hero's pack
    , title : String
    }


cardCount : Deck -> Int
cardCount deck =
    List.length deck.cards + List.length deck.heroCards


decoder : Decoder Deck
decoder =
    Json.Decode.succeed Deck
        |> required "id" Json.Decode.string
        |> required "affinities" (Json.Decode.list Faction.decoder)
        |> required "cards" Card.cardListDecoder
        |> required "hero" Card.decoder
        |> required "heroCards" Card.cardListDecoder
        |> required "title" Json.Decode.string


encode : Deck -> Json.Encode.Value
encode deck =
    Json.Encode.object <|
        [ ( "id", Json.Encode.string deck.id )
        , ( "affinities", Json.Encode.list Faction.encode deck.affinities )
        , ( "cards", Json.Encode.list Card.encodeCard deck.cards )
        , ( "hero", Card.encodeCard deck.hero )
        , ( "heroCards", Json.Encode.list Card.encodeCard deck.heroCards )
        , ( "title", Json.Encode.string deck.title )
        ]


encodeDeckCreationPayload : Deck -> Json.Encode.Value
encodeDeckCreationPayload deck =
    Json.Encode.object <|
        [ ( "affinities", Json.Encode.list Faction.encode deck.affinities )
        , ( "cards", Json.Encode.list Card.encodeCard deck.cards )
        , ( "hero", Card.encodeCard deck.hero )
        , ( "heroCards", Json.Encode.list Card.encodeCard deck.heroCards )
        , ( "title", Json.Encode.string deck.title )
        ]
