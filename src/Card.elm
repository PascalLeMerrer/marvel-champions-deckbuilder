module Card exposing (Card, cardListDecoder, decoder, encodeCard, encodeNewCard, viewCardsTable)

import Button exposing (button)
import Colors exposing (black, charcoal, darkGreen, darkerGreen, grey, lightGrey, lighterGreen, white)
import Element as E exposing (px, spacing)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Faction exposing (Faction)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Kind exposing (Kind)
import Table


type alias Card =
    { id : String
    , cardSetCode : String
    , code : String
    , imagesrc : Maybe String
    , isImageVisible : Bool
    , isDuplicateOf : Maybe String
    , kind : Kind
    , name : String
    , quantity : Int
    , faction : Faction
    , selectedQuantity : Int -- in a deck
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
        |> required "selectedQuantity" Json.Decode.int


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
        , ( "selectedQuantity", Encode.int card.selectedQuantity )
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
        , ( "selectedQuantity", Encode.int card.selectedQuantity )
        ]



-- View --


type alias Options msg =
    { showCount : Bool
    , action : Maybe (E.Element msg)
    , selectMsg :
        Card
        -> msg -- TOD distinguer click pour sélectionner et click pour afficher/masquer l'image
    , unselectMsg : Card -> msg
    , quantityChangedMsg : Maybe (Card -> Int -> msg)
    }


viewCardsTable : List Card -> Options msg -> E.Element msg
viewCardsTable cards options =
    let
        sortedCards : List Card
        sortedCards =
            List.sortBy .code cards

        countColumn : List (E.IndexedColumn Card msg)
        countColumn =
            case ( options.showCount, options.quantityChangedMsg ) of
                ( True, Nothing ) ->
                    [ { header = E.el Table.headerAttributes (E.text "Quantité")
                      , width = E.fill
                      , view = \index card -> E.el (rowAttributes index card) (E.text <| String.fromInt card.quantity)
                      }
                    ]

                ( False, Nothing ) ->
                    []

                ( _, Just quantityChangedMsg ) ->
                    [ { header = E.el Table.headerAttributes (E.text "Quantité")
                      , width = E.fill
                      , view =
                            \index card ->
                                E.el (rowAttributes index card)
                                    (viewQuantity quantityChangedMsg card)
                      }
                    ]
    in
    if List.length sortedCards > 0 then
        E.indexedTable
            [ Border.color charcoal
            , Font.size 11
            , Font.color white
            ]
            { data = sortedCards
            , columns =
                [ { header = E.el Table.headerAttributes (E.text "Carte")
                  , width = E.fill
                  , view = viewImage options
                  }
                , { header = E.el Table.headerAttributes (E.text "Code")
                  , width = E.fill
                  , view =
                        \index card ->
                            E.el (rowAttributes index card) (E.text card.code)
                  }
                , { header = E.el Table.headerAttributes (E.text "Nom")
                  , width = E.fill
                  , view =
                        \index card ->
                            E.el (rowAttributes index card) (E.text card.name)
                  }
                , { header = E.el Table.headerAttributes (E.text "Type")
                  , width = E.fill
                  , view =
                        \index card ->
                            E.el (rowAttributes index card) (E.text <| Kind.toString card.kind)
                  }
                ]
                    ++ countColumn
            }

    else
        E.none


rowAttributes : Int -> Card -> List (E.Attribute msg)
rowAttributes index card =
    [ Background.color <|
        if modBy 2 index == 0 then
            grey

        else
            lightGrey
    , E.mouseOver
        [ if card.isImageVisible then
            Font.color white

          else
            Font.color darkerGreen
        ]
    , E.alignTop
    , E.padding 5
    , E.pointer
    , E.height E.fill
    , Font.color black
    ]
        ++ (if card.isImageVisible then
                [ Background.color lighterGreen
                , Font.heavy
                ]

            else
                []
           )


viewImage : Options msg -> Int -> Card -> E.Element msg
viewImage options index card =
    let
        attributes =
            rowAttributes index card

        makeClickable : List (E.Attr () msg) -> List (E.Attr () msg)
        makeClickable attributeList =
            (if card.isImageVisible then
                onClick (options.unselectMsg card)

             else
                onClick (options.selectMsg card)
            )
                :: attributeList

        addImageDimensions : List (E.Attribute msg) -> List (E.Attribute msg)
        addImageDimensions attributeList =
            [ E.width (px 300), E.height (px 400) ] ++ attributeList
    in
    case ( card.imagesrc, card.isImageVisible ) of
        ( _, False ) ->
            E.el (Font.italic :: attributes |> makeClickable) <| E.text "Cliquer pour afficher l'image"

        ( Nothing, True ) ->
            E.el (Font.italic :: attributes) <| E.text "Image non disponible"

        ( Just path, True ) ->
            E.image
                (attributes |> makeClickable |> addImageDimensions)
                { src = "https://fr.marvelcdb.com" ++ path, description = card.name }


viewQuantity : (Card -> Int -> msg) -> Card -> E.Element msg
viewQuantity msg card =
    List.range 0 card.quantity
        |> List.map (\quantity -> viewQuantityButton card.selectedQuantity quantity (msg card))
        |> E.row [ spacing 5 ]


viewQuantityButton : Int -> Int -> (Int -> msg) -> E.Element msg
viewQuantityButton selectedQuantity index msg =
    let
        label =
            String.fromInt index

        kind =
            if selectedQuantity == index then
                Button.Primary

            else
                Button.Secondary
    in
    button kind label 25 10 <| msg index
