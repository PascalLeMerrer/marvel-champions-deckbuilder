module Pack exposing (Pack, PackStatus(..), encodeNewPack, packDecoder, packListDecoder)

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode


type PackStatus
    = Received
    | Saved


type alias Pack =
    { id : String
    , marvelCdbId : Int
    , name : String
    , status : PackStatus
    }



{- Encoders/ decoders for interactions with the backend -}


packDecoder : Decoder Pack
packDecoder =
    Json.Decode.succeed Pack
        |> required "id" Json.Decode.string
        |> required "marvel_cdb_id" Json.Decode.int
        |> required "name" Json.Decode.string
        |> hardcoded Saved


packListDecoder : Decoder (List Pack)
packListDecoder =
    Json.Decode.field "data" <|
        Json.Decode.list packDecoder


encodePack : Pack -> Encode.Value
encodePack pack =
    Encode.object <|
        [ ( "id", Encode.string pack.id )
        , ( "marvel_cdb_id", Encode.int pack.marvelCdbId )
        , ( "name", Encode.string pack.name )
        ]


encodeNewPack : Pack -> Encode.Value
encodeNewPack pack =
    -- when creating a new pack, we cannot pass an ID; the backend will generate it
    Encode.object <|
        [ ( "name", Encode.string pack.name )
        , ( "marvel_cdb_id", Encode.int pack.marvelCdbId )
        ]
