module Affinity exposing (Affinity, aggression, all, justice, leadership, name, protection, toString)


type Kind
    = Aggression
    | Leadership
    | Justice
    | Protection


type Affinity
    = Affinity String


aggression : Affinity
aggression =
    Affinity "aggression"


leadership : Affinity
leadership =
    Affinity "leadership"


justice : Affinity
justice =
    Affinity "justice"


protection : Affinity
protection =
    Affinity "protection"


all : List Affinity
all =
    [ aggression, leadership, justice, protection ]


name : Affinity -> String
name (Affinity affinity) =
    affinity


fromString : String -> Affinity
fromString str =
    case str of
        "aggression" ->
            aggression

        "leadership" ->
            leadership

        "justice" ->
            justice

        _ ->
            protection


toString : Affinity -> String
toString affinity =
    case affinity of
        Affinity "aggression" ->
            "Aggressivité"

        Affinity "leadership" ->
            "Commandement"

        Affinity "justice" ->
            "Justice"

        Affinity _ ->
            "Protection"
