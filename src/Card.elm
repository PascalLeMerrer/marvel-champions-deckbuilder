module Card exposing (Card, cardListDecoder, decoder, encodeCard, encodeNewCard, viewCardsTable)

import Element as E exposing (px, rgb255)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Faction exposing (Faction)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Kind exposing (Kind)


type alias Card =
    { id : String
    , cardSetCode : String
    , code : String
    , imagesrc : Maybe String
    , isSelected : Bool
    , isDuplicateOf : Maybe String
    , kind : Kind
    , name : String
    , quantity : Int
    , faction : Faction
    }



{- Encoders/ decoders for interactions with the backend -}


decoder : Decoder Card
decoder =
    Json.Decode.succeed Card
        |> required "id" Json.Decode.string
        |> required "cardSetCode" Json.Decode.string
        |> required "code" Json.Decode.string
        |> optional "imagesrc" (Json.Decode.map Just Json.Decode.string) Nothing
        |> hardcoded False
        |> optional "isDuplicateOf" (Json.Decode.map Just Json.Decode.string) Nothing
        |> required "kind" Kind.decoder
        |> required "name" Json.Decode.string
        |> required "quantity" Json.Decode.int
        |> required "faction" Faction.decoder


cardListDecoder : Decoder (List Card)
cardListDecoder =
    Json.Decode.list decoder


encodeCard : Card -> Encode.Value
encodeCard card =
    Encode.object <|
        [ ( "id", Encode.string card.id )
        , ( "cardSetCode", Encode.string card.cardSetCode )
        , ( "code", Encode.string card.code )
        , ( "faction", Faction.encode card.faction )
        , ( "kind", Kind.encode card.kind )
        , ( "imagesrc"
          , case card.imagesrc of
                Just url ->
                    Encode.string url

                Nothing ->
                    Encode.null
          )
        , ( "isDuplicateOf"
          , case card.isDuplicateOf of
                Just code ->
                    Encode.string code

                Nothing ->
                    Encode.null
          )
        , ( "name", Encode.string card.name )
        , ( "quantity", Encode.int card.quantity )
        ]


encodeNewCard : Card -> Encode.Value
encodeNewCard card =
    -- when creating a new card, we cannot pass an ID; the backend will generate it
    -- TODO find a pattern to avoid this code duplication
    Encode.object <|
        [ ( "code", Encode.string card.code )
        , ( "cardSetCode", Encode.string card.cardSetCode )
        , ( "kind", Kind.encode card.kind )
        , ( "faction", Faction.encode card.faction )
        , ( "imagesrc"
          , case card.imagesrc of
                Just url ->
                    Encode.string url

                Nothing ->
                    Encode.null
          )
        , ( "isDuplicateOf"
          , case card.isDuplicateOf of
                Just code ->
                    Encode.string code

                Nothing ->
                    Encode.null
          )
        , ( "name", Encode.string card.name )
        , ( "quantity", Encode.int card.quantity )
        ]



-- View --


viewCardsTable : List Card -> (Card -> msg) -> (Card -> msg) -> E.Element msg
viewCardsTable cards selectMsg unselectMsg =
    let
        sortedCards : List Card
        sortedCards =
            List.sortBy .code cards
    in
    if List.length sortedCards > 0 then
        E.indexedTable
            [ Border.color (rgb255 3 95 22)
            , Border.solid
            , Border.width 1
            , Font.size 11
            ]
            { data = sortedCards
            , columns =
                [ { header = E.el headerAttributes (E.text "Carte")
                  , width = E.fill
                  , view = viewCard selectMsg unselectMsg
                  }
                , { header = E.el headerAttributes (E.text "Code")
                  , width = E.fill
                  , view =
                        \index card ->
                            E.el (rowAttributes selectMsg unselectMsg index card) (E.text card.code)
                  }
                , { header = E.el headerAttributes (E.text "Nom")
                  , width = E.fill
                  , view =
                        \index card ->
                            E.el (rowAttributes selectMsg unselectMsg index card) (E.text card.name)
                  }
                , { header = E.el headerAttributes (E.text "Type")
                  , width = E.fill
                  , view =
                        \index card ->
                            E.el (rowAttributes selectMsg unselectMsg index card) (E.text <| Kind.toString card.kind)
                  }
                ]
            }

    else
        E.text ""


rowAttributes : (Card -> msg) -> (Card -> msg) -> Int -> Card -> List (E.Attribute msg)
rowAttributes selectMsg unselectMsg index card =
    [ Background.color <|
        if modBy 2 index == 0 then
            rgb255 205 203 203

        else
            rgb255 246 244 244
    , E.alignTop
    , E.padding 5
    , E.pointer
    ]
        ++ (if card.isSelected then
                [ Background.color <| rgb255 205 238 150
                , Font.heavy
                , onClick (unselectMsg card)
                ]

            else
                [ onClick (selectMsg card) ]
           )


headerAttributes : List (E.Attribute msg)
headerAttributes =
    [ Background.color <| rgb255 31 199 170
    , E.alignTop
    , Font.bold
    , E.padding 5
    , Border.widthEach
        { bottom = 1
        , left = 0
        , right = 0
        , top = 0
        }
    , Border.color <| rgb255 62 62 62
    ]


viewCard : (Card -> msg) -> (Card -> msg) -> Int -> Card -> E.Element msg
viewCard selectMsg unselectMsg index card =
    let
        attributes =
            rowAttributes selectMsg unselectMsg index card
    in
    case ( card.imagesrc, card.isSelected ) of
        ( _, False ) ->
            E.el (Font.italic :: attributes) <| E.text "Cliquer pour afficher l'image"

        ( Nothing, True ) ->
            E.el (Font.italic :: attributes) <| E.text "Cliquer pour afficher l'image"

        ( Just path, True ) ->
            E.image
                (attributes ++ [ E.width (px 300), E.height (px 400) ])
                { src = "https://fr.marvelcdb.com" ++ path, description = card.name }
