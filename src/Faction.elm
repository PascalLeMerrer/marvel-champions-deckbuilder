module Faction exposing (Faction, aggression, allAffinities, basic, decoder, encode, hero, justice, leadership, protection, toString)

import Json.Decode exposing (Decoder)
import Json.Encode


type Faction
    = Faction String


aggression : Faction
aggression =
    Faction "aggression"


basic : Faction
basic =
    Faction "basic"


hero : Faction
hero =
    Faction "hero"


justice : Faction
justice =
    Faction "justice"


leadership : Faction
leadership =
    Faction "leadership"


protection : Faction
protection =
    Faction "protection"


allAffinities : List Faction
allAffinities =
    [ aggression, leadership, justice, protection ]


toString : Faction -> String
toString faction =
    case faction of
        Faction "aggression" ->
            "Aggressivité"

        Faction "leadership" ->
            "Commandement"

        Faction "justice" ->
            "Justice"

        Faction "protection" ->
            "Protection"

        Faction "hero" ->
            "Héros"

        Faction _ ->
            "Basique"


decoder : Decoder Faction
decoder =
    Json.Decode.map Faction Json.Decode.string


encode : Faction -> Json.Encode.Value
encode (Faction string) =
    Json.Encode.string string
