module Pages.Deck.Id_ exposing (Model, Msg, page)

import Backend exposing (createDeckCmd, getDeckCmd, saveDeckCmd)
import Card exposing (Card, viewCardsTable)
import Deck exposing (Deck)
import Element as E
import Element.Font as Font
import Element.Input as Input
import Error exposing (viewError)
import Faction exposing (Faction, basic)
import Gen.Params.Deck.Id_ exposing (Params)
import Header
import Kinto
import List.Extra exposing (updateIf)
import Page
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init shared req
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { allCards : List Card
    , availableCards : List Card
    , cardSearchResult : List Card
    , cardSearchText : String
    , deck : Maybe Deck
    , error : Maybe String
    , selectedCards : List Card
    }


withAvailableCards : List Card -> Model -> Model
withAvailableCards cards model =
    { model | availableCards = cards }


withSelectedCards : List Card -> Model -> Model
withSelectedCards cards model =
    case model.deck of
        Just deck ->
            { model
                | selectedCards = Debug.log "New cards in deck" cards
                , deck = Just { deck | cards = cards }
            }

        Nothing ->
            model


init : Shared.Model -> Request.With Params -> ( Model, Cmd Msg )
init shared req =
    ( { allCards = shared.cards
      , availableCards = filter [] shared.cards
      , cardSearchResult = []
      , cardSearchText = ""
      , deck = Nothing
      , error = Nothing
      , selectedCards = [] -- TODO load from DB if the deck already exists
      }
    , getDeckCmd req.params.id BackendReturnedDeck
    )



-- UPDATE


type Msg
    = BackendReturnedDeck (Result Kinto.Error Deck)
    | UserChangedCardSearchText String
    | UserClickedUnselectedCard Card
    | UserClickedSelectedCard Card
    | UserChangedCardQuantity Card Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserChangedCardSearchText searchText ->
            ( model |> searchCard searchText
            , Cmd.none
            )

        UserClickedUnselectedCard card ->
            ( { model
                | cardSearchResult =
                    List.map (\c -> { c | isImageVisible = c == card }) model.cardSearchResult
              }
            , Cmd.none
            )

        UserClickedSelectedCard card ->
            ( { model
                | cardSearchResult =
                    updateIf (\c -> c == card) (\_ -> { card | isImageVisible = False }) model.cardSearchResult
              }
            , Cmd.none
            )

        UserChangedCardQuantity card quantity ->
            let
                newCard =
                    { card | selectedQuantity = quantity }

                selectedCards =
                    if List.member card model.selectedCards then
                        if quantity == 0 then
                            --remove the card from the selection
                            List.filter (\c -> c.id /= card.id) model.selectedCards

                        else
                            List.Extra.updateIf (\c -> c.id == card.id) (\c -> newCard) model.selectedCards

                    else
                        newCard :: model.selectedCards

                availableCards =
                    if quantity == 0 then
                        if List.member card model.availableCards then
                            -- we reset to 0 the quantity of an available card
                            List.Extra.updateIf (\c -> c.id == card.id) (\c -> newCard) model.availableCards

                        else
                            -- the card is just removed from selection, we add it back to available cards
                            newCard :: model.availableCards

                    else
                        -- the card is moved to selectedCards, we remove it from available cards
                        List.filter (\c -> c.id /= card.id) model.availableCards

                updatedModel =
                    model
                        |> withAvailableCards availableCards
                        |> withSelectedCards selectedCards
                        |> searchCard model.cardSearchText
            in
            ( updatedModel
            , case updatedModel.deck of
                Just deck ->
                    saveDeckCmd deck BackendReturnedDeck

                Nothing ->
                    Cmd.none
            )

        BackendReturnedDeck (Ok deck) ->
            ( { model
                | deck = Just deck
                , availableCards = filter deck.affinities model.allCards
                , selectedCards = deck.cards
              }
            , Cmd.none
            )

        BackendReturnedDeck (Err kintoError) ->
            ( { model | error = Just (Kinto.errorToString kintoError) }
            , Cmd.none
            )


filter : List Faction -> List Card -> List Card
filter affinities cards =
    let
        selected_factions : List Faction
        selected_factions =
            basic :: affinities
    in
    cards
        |> List.filter
            (\card ->
                List.member card.faction selected_factions
            )


searchCard : String -> Model -> Model
searchCard searchText model =
    let
        lowercaseSearchText =
            String.toLower searchText

        filterCards card =
            String.startsWith lowercaseSearchText (String.toLower card.name)

        searchResult =
            if not (String.isEmpty searchText) then
                List.filter filterCards model.availableCards

            else
                []
    in
    { model
        | cardSearchText = searchText
        , cardSearchResult = searchResult
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Import"
    , body =
        [ E.layout
            [ E.width E.fill, E.height E.fill, E.inFront (Header.view <| Just "Modifier un deck") ]
          <|
            E.el
                [ E.paddingXY 20 80
                ]
                (E.column
                    [ Font.size 11
                    , E.spacing 20
                    , E.width E.fill
                    ]
                    [ viewError model
                    , viewDeckTitle model
                    , viewSubtitle "Rechercher une carte"
                    , viewCardSearch model
                    , viewCardsTable model.cardSearchResult
                        { showCount = True
                        , action = Nothing
                        , selectMsg = UserClickedUnselectedCard
                        , unselectMsg = UserClickedSelectedCard
                        , quantityChangedMsg = Just UserChangedCardQuantity
                        }
                    , if List.isEmpty model.selectedCards then
                        E.none

                      else
                        viewSubtitle "Cartes dans le deck"
                    , viewCardsTable model.selectedCards
                        { showCount = True
                        , action = Nothing
                        , selectMsg = UserClickedUnselectedCard -- FIXME
                        , unselectMsg = UserClickedSelectedCard -- FIXME
                        , quantityChangedMsg = Just UserChangedCardQuantity
                        }
                    ]
                )
        ]
    }


viewDeckTitle : Model -> E.Element Msg
viewDeckTitle model =
    case model.deck of
        Just deck ->
            E.el
                [ Font.size 20
                , Font.bold
                ]
            <|
                E.text deck.title

        Nothing ->
            E.none


viewCardSearch : Model -> E.Element Msg
viewCardSearch model =
    Input.search
        [ E.padding 6
        , E.width (E.px 200)
        ]
        { onChange = UserChangedCardSearchText
        , text = model.cardSearchText
        , placeholder = Just <| Input.placeholder [] (E.text "Nom de la carte")
        , label = Input.labelAbove [] (E.text "Carte")
        }


viewSubtitle : String -> E.Element msg
viewSubtitle subtitle =
    E.el
        [ Font.size 14
        , Font.bold
        ]
    <|
        E.text subtitle
