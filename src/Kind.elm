module Kind exposing (..)

{--Represents a card kind - Matches he type_code of the Marvel CDB API --}

import Json.Decode exposing (Decoder)
import Json.Encode


type Kind
    = Kind String


hero : Kind
hero =
    Kind "hero"


ally : Kind
ally =
    Kind "ally"


support : Kind
support =
    Kind "support"


upgrade : Kind
upgrade =
    Kind "upgrade"


event : Kind
event =
    Kind "event"


alter_ego : Kind
alter_ego =
    Kind "alter_ego"


resource : Kind
resource =
    Kind "resource"


decoder : Decoder Kind
decoder =
    Json.Decode.map Kind Json.Decode.string


encode : Kind -> Json.Encode.Value
encode (Kind value) =
    Json.Encode.string value


toString : Kind -> String
toString kind =
    case kind of
        Kind "hero" ->
            "Héros"

        Kind "ally" ->
            "Allié"

        Kind "support" ->
            "Soutien"

        Kind "upgrade" ->
            "Amélioration"

        Kind "event" ->
            "Événement"

        Kind "alter_ego" ->
            "Alter ego"

        Kind _ ->
            "Ressource"
