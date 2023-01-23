module Pack exposing (Pack, PackStatus(..), encodeNewPack, newPackListDecoder, packDecoder)

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode


type PackStatus
    = Received
    | Saved


type alias Pack =
    { id : String
    , marvel_cdb_id : Int
    , name : String
    , status : PackStatus
    }



{- Encoders/ decoders for interactions with MarvelCDB -}


newPackListDecoder : Decoder (List Pack)
newPackListDecoder =
    Json.Decode.list newPackDecoder


newPackDecoder : Decoder Pack
newPackDecoder =
    Json.Decode.succeed Pack
        |> hardcoded ""
        |> required "id" Json.Decode.int
        |> required "name" Json.Decode.string
        |> hardcoded Received



{- Encoders/ decoders for interactions with the backend -}


packDecoder : Decoder Pack
packDecoder =
    Json.Decode.succeed Pack
        |> required "id" Json.Decode.string
        |> required "marvel_dcb_id" Json.Decode.int
        |> required "name" Json.Decode.string
        |> hardcoded Saved


encodePack : Pack -> Encode.Value
encodePack pack =
    Encode.object <|
        [ ( "id", Encode.string pack.id )
        , ( "marvel_dcb_id", Encode.int pack.marvel_cdb_id )
        , ( "name", Encode.string pack.name )
        ]


encodeNewPack : Pack -> Encode.Value
encodeNewPack pack =
    -- when creating a new pack, we cannot pass an ID; the backend will generate it
    Encode.object <|
        [ ( "name", Encode.string pack.name )
        , ( "marvel_dcb_id", Encode.int pack.marvel_cdb_id )
        ]
