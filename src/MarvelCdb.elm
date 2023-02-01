module MarvelCdb exposing (loadAllCardsFromMarvelCdbCmd, loadAllPacksFromMarvelCdbCmd, marvelCDBName)

import Card exposing (Card)
import Faction
import Http
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Kind
import Pack exposing (Pack, PackStatus(..))


marvelCDBName : String
marvelCDBName =
    "The Marvel CDB server"


cardsUrl =
    "https://fr.marvelcdb.com/api/public/cards/"


packsUrl =
    "https://fr.marvelcdb.com/api/public/packs/"


loadAllPacksFromMarvelCdbCmd : (Result Http.Error (List Pack) -> msg) -> Cmd msg
loadAllPacksFromMarvelCdbCmd msg =
    Http.get
        { url = packsUrl
        , expect = Http.expectJson msg newPackListDecoder
        }


loadAllCardsFromMarvelCdbCmd : (Result Http.Error (List Card) -> msg) -> Cmd msg
loadAllCardsFromMarvelCdbCmd msg =
    Http.get
        { url = cardsUrl
        , expect = Http.expectJson msg newCardListDecoder
        }


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


newCardListDecoder : Decoder (List Card)
newCardListDecoder =
    Json.Decode.list newCardDecoder


newCardDecoder : Decoder Card
newCardDecoder =
    Json.Decode.succeed Card
        |> hardcoded ""
        |> optional "card_set_code" Json.Decode.string ""
        |> required "code" Json.Decode.string
        |> optional "imagesrc" (Json.Decode.map Just Json.Decode.string) (Just "no url")
        |> hardcoded False
        |> optional "duplicate_of_code" (Json.Decode.map Just Json.Decode.string) Nothing
        |> required "type_code" Kind.decoder
        |> required "name" Json.Decode.string
        |> required "quantity" Json.Decode.int
        |> required "faction_code" Faction.decoder
