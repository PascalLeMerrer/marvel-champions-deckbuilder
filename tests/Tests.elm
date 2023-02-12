module Tests exposing (suite)

import Card exposing (Card)
import Expect
import Faction
import Fuzz
import Kind
import Pages.Deck.Id_ exposing (Model, Msg(..), update, withAllCards, withAvailableCards, withSearchText, withSelectedCards)
import Test exposing (Test, describe, fuzz, test)


hero : Card
hero =
    { id = "heroCard1"
    , cardSetCode = "card set code 1"
    , code = "code 1"
    , imagesrc = Nothing
    , isImageVisible = False
    , isDuplicateOf = Nothing
    , kind = Kind.event
    , name = "The hero card"
    , quantity = 1
    , faction = Faction.hero
    , selectedQuantity = 0
    }


card1 =
    { id = "card1"
    , cardSetCode = "card set code 1"
    , code = "code 1"
    , imagesrc = Nothing
    , isImageVisible = False
    , isDuplicateOf = Nothing
    , kind = Kind.event
    , name = "The first card"
    , quantity = 1
    , faction = Faction.hero
    , selectedQuantity = 0
    }


card2 =
    { id = "card2"
    , cardSetCode = "card set code 2"
    , code = "code 2"
    , imagesrc = Nothing
    , isImageVisible = False
    , isDuplicateOf = Nothing
    , kind = Kind.support
    , name = "The second card"
    , quantity = 3
    , faction = Faction.leadership
    , selectedQuantity = 0
    }


card3 =
    { id = "card3"
    , cardSetCode = "card set code 1"
    , code = "code 1"
    , imagesrc = Nothing
    , isImageVisible = False
    , isDuplicateOf = Nothing
    , kind = Kind.ally
    , name = "The third card"
    , quantity = 2
    , faction = Faction.basic
    , selectedQuantity = 0
    }


deck =
    { id = "deck1"
    , affinities = [ Faction.leadership ]
    , cards = []
    , hero = hero
    , heroCards = []
    , title = "My deck"
    }


baseModel : Model
baseModel =
    { allCards = [ card1, card2, card3 ]
    , availableCards = []
    , cardSearchResult = []
    , cardSearchText = ""
    , deck = Just deck
    , error = Nothing
    , selectedCards = []
    }


suite : Test
suite =
    describe "Change quantity of a given card in deck"
        [ fuzz (Fuzz.intRange 1 3)
            "Setting the selected quantity of a card to a number greater than 0 moves it to selectedCards"
          <|
            \cardCount ->
                let
                    ( model, _ ) =
                        update (UserChangedCardQuantity card1 cardCount) baseModel

                    expected : List Card
                    expected =
                        [ { card1 | selectedQuantity = cardCount } ]
                in
                Expect.equalLists expected model.selectedCards
        , test
            "Setting the selected quantity of an available card to 0 does not remove it from available cards"
          <|
            \_ ->
                let
                    ( model, _ ) =
                        baseModel
                            |> withAvailableCards [ card1, card2, card3 ]
                            |> update (UserChangedCardQuantity card1 0)
                in
                Expect.equalLists [ card1, card2, card3 ] model.availableCards
        , test
            "Setting the selected quantity of an available card to 0 does not add it to  selected cards"
          <|
            \_ ->
                let
                    ( model, _ ) =
                        baseModel
                            |> withAvailableCards [ card1, card2, card3 ]
                            |> update (UserChangedCardQuantity card1 0)
                in
                Expect.equalLists [] model.selectedCards
        , fuzz (Fuzz.intRange 1 3)
            "Setting the selected quantity of a card to a number greater than 0 removes it from available cards"
          <|
            \cardCount ->
                let
                    ( model, _ ) =
                        baseModel
                            |> withAvailableCards [ card1, card2, card3 ]
                            |> update (UserChangedCardQuantity card2 cardCount)
                in
                Expect.equalLists [ card1, card3 ] model.availableCards
        , fuzz (Fuzz.intRange 1 3)
            "Setting selected card quantity to 0 adds it to available cards"
          <|
            \cardCount ->
                let
                    card =
                        { card1 | selectedQuantity = cardCount }

                    ( model, _ ) =
                        baseModel
                            |> withAvailableCards [ card3 ]
                            |> withSelectedCards
                                [ card
                                , { card2 | selectedQuantity = cardCount }
                                ]
                            |> update (UserChangedCardQuantity card 0)
                in
                Expect.equalLists [ card1, card3 ] model.availableCards
        , fuzz (Fuzz.intRange 1 3)
            "Setting selected card quantity to 0 removes it from selected cards"
          <|
            \cardCount ->
                let
                    selectedCard1 : Card
                    selectedCard1 =
                        { card1 | selectedQuantity = cardCount }

                    selectedCard2 : Card
                    selectedCard2 =
                        { card2 | selectedQuantity = cardCount }

                    ( model, _ ) =
                        baseModel
                            |> withAvailableCards [ card3 ]
                            |> withSelectedCards
                                [ selectedCard1
                                , selectedCard2
                                ]
                            |> update (UserChangedCardQuantity selectedCard1 0)
                in
                Expect.equalLists [ card1, card3 ] model.availableCards
        , fuzz (Fuzz.intRange 1 3)
            "Setting selected card quantity to 0 adds it to search result if its name matches the searched string"
          <|
            \cardCount ->
                let
                    card =
                        { card1 | selectedQuantity = cardCount }

                    ( model, _ ) =
                        baseModel
                            |> withAvailableCards [ card3 ]
                            |> withSelectedCards
                                [ card
                                , { card2 | selectedQuantity = cardCount }
                                ]
                            |> withSearchText (String.left 6 card.name)
                            |> update (UserChangedCardQuantity card 0)
                in
                Expect.equalLists [ card1 ] model.cardSearchResult
        , fuzz (Fuzz.intRange 1 3)
            "Setting selected card quantity to 0 does no add it to search result if its name does not match the searched string"
          <|
            \cardCount ->
                let
                    card =
                        { card1 | selectedQuantity = cardCount }

                    ( model, _ ) =
                        baseModel
                            |> withAvailableCards [ card3 ]
                            |> withSelectedCards
                                [ card
                                , { card2 | selectedQuantity = cardCount }
                                ]
                            |> withSearchText (String.left 6 card3.name)
                            |> update (UserChangedCardQuantity card 0)
                in
                Expect.equalLists [ card3 ] model.cardSearchResult
        , fuzz (Fuzz.intRange 2 4)
            "Reducing selected card quantity to a number greater than 0 does no add it to search results"
          <|
            \cardCount ->
                let
                    card =
                        { card1 | selectedQuantity = cardCount }

                    ( model, _ ) =
                        baseModel
                            |> withAvailableCards [ card3 ]
                            |> withSelectedCards
                                [ card
                                , { card2 | selectedQuantity = cardCount }
                                ]
                            |> withSearchText (String.left 6 card.name)
                            |> update (UserChangedCardQuantity card (cardCount - 1))
                in
                Expect.equalLists [] model.cardSearchResult
        ]
