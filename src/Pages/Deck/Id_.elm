module Pages.Deck.Id_ exposing (Model, Msg(..), page, update, withAllCards, withAvailableCards, withSearchText, withSelectedCards)

import Backend exposing (KintoData, getDeckCmd, saveDeckCmd)
import Card exposing (Card, viewCardsTable)
import Deck exposing (Deck)
import Effect exposing (Effect)
import Element as E
import Element.Font as Font
import Element.Input as Input
import Error exposing (viewError)
import Faction exposing (Faction, basic)
import Gen.Params.Deck.Id_ exposing (Params)
import Gen.Route as Route
import Header
import Kinto
import List.Extra exposing (updateIf)
import Page
import RemoteData exposing (RemoteData(..))
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
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
    , deck : KintoData Deck
    , error : Maybe String
    , selectedCards : List Card
    }


emptyModel : Model
emptyModel =
    { allCards = []
    , availableCards = []
    , cardSearchResult = []
    , cardSearchText = ""
    , deck = NotAsked
    , error = Nothing
    , selectedCards = []
    }


withAllCards : List Card -> Model -> Model
withAllCards cards model =
    { model | allCards = cards }


withAvailableCards : List Card -> Model -> Model
withAvailableCards cards model =
    { model | availableCards = cards }


withSearchText : String -> Model -> Model
withSearchText searchText model =
    { model | cardSearchText = searchText }


withSelectedCards : List Card -> Model -> Model
withSelectedCards cards model =
    case model.deck of
        Success deck ->
            { model
                | selectedCards = cards
                , deck = Success { deck | cards = cards }
            }

        _ ->
            model


init : Shared.Model -> Request.With Params -> ( Model, Effect Msg )
init shared req =
    case shared.status of
        Shared.Error ->
            ( emptyModel
            , Effect.none
            )

        Shared.Loaded ->
            ( emptyModel
                |> withAllCards shared.cards
                |> filterAvailableCards
            , Effect.fromCmd <| getDeckCmd req.params.id (RemoteData.fromResult >> BackendReturnedDeck)
            )

        _ ->
            ( emptyModel
            , Effect.fromCmd <|
                Request.replaceRoute Route.Home_ req
            )



-- UPDATE


type Msg
    = BackendReturnedDeck (KintoData Deck)
    | UserChangedCardSearchText String
    | UserClickedUnselectedCard Card
    | UserClickedSelectedCard Card
    | UserChangedCardQuantity Card Int


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UserChangedCardSearchText searchText ->
            ( model |> searchCard searchText
            , Effect.none
            )

        UserClickedUnselectedCard card ->
            ( { model
                | cardSearchResult =
                    List.map (\c -> { c | isImageVisible = c == card }) model.cardSearchResult
              }
            , Effect.none
            )

        UserClickedSelectedCard card ->
            ( { model
                | cardSearchResult =
                    updateIf (\c -> c == card) (\_ -> { card | isImageVisible = False }) model.cardSearchResult
              }
            , Effect.none
            )

        UserChangedCardQuantity card newQuantity ->
            let
                newCard =
                    { card | selectedQuantity = newQuantity }

                selectedCards =
                    case ( card.selectedQuantity, newQuantity ) of
                        ( 0, 0 ) ->
                            model.selectedCards

                        ( _, 0 ) ->
                            -- remove the card from the selection
                            List.filter (\c -> c.id /= card.id) model.selectedCards

                        ( 0, _ ) ->
                            -- add the card to selection
                            newCard :: model.selectedCards

                        ( _, _ ) ->
                            -- The card was already selected, and we changed its quantity to a value greater than 0
                            List.Extra.updateIf (\c -> c.id == card.id) (\_ -> newCard) model.selectedCards

                availableCards =
                    case ( card.selectedQuantity, newQuantity ) of
                        ( 0, 0 ) ->
                            -- The card was not selected, and we did not change its quantity so it remains unchanged
                            model.availableCards

                        ( _, 0 ) ->
                            -- the card was selected, now it's not; add it to available cards
                            newCard :: model.availableCards

                        ( 0, _ ) ->
                            -- the card was not selected, now it is; remove it from available cards
                            List.filter (\c -> c.id /= card.id) model.availableCards

                        ( _, _ ) ->
                            -- The card was already selected, and we changed its quantity to a value greater than 0
                            -- so it remains unchanged
                            model.availableCards

                updatedModel =
                    model
                        |> withAvailableCards availableCards
                        |> withSelectedCards selectedCards
                        |> searchCard model.cardSearchText
            in
            ( updatedModel
            , case updatedModel.deck of
                Success deck ->
                    Effect.fromCmd <| saveDeckCmd deck (RemoteData.fromResult >> BackendReturnedDeck)

                _ ->
                    Effect.none
            )

        BackendReturnedDeck (Success deck) ->
            ( { model
                | deck = Success deck
                , selectedCards = deck.cards
              }
                |> filterAvailableCards
            , Effect.none
            )

        BackendReturnedDeck (Failure kintoError) ->
            ( { model | error = Just (Kinto.errorToString kintoError) }
            , Effect.none
            )

        BackendReturnedDeck _ ->
            ( { model | error = Nothing }
            , Effect.none
            )


filterAvailableCards : Model -> Model
filterAvailableCards model =
    let
        selectedFactions : List Faction
        selectedFactions =
            case model.deck of
                Success deck ->
                    basic :: deck.affinities

                _ ->
                    [ basic ]

        selectedCardIds : List String
        selectedCardIds =
            List.map .id model.selectedCards
    in
    { model
        | availableCards =
            model.allCards
                |> List.filter
                    (\card ->
                        List.member card.faction selectedFactions
                            && not (List.member card.id selectedCardIds)
                    )
    }


searchCard : String -> Model -> Model
searchCard searchText model =
    let
        lowercaseSearchText =
            String.toLower searchText

        filterCards card =
            String.startsWith lowercaseSearchText (String.toLower card.name)

        searchResult =
            Debug.log "searchResult" <|
                if not (String.isEmpty searchText) then
                    List.filter filterCards (Debug.log "model.availableCards" model.availableCards)

                else
                    []
    in
    { model
        | cardSearchText = searchText
        , cardSearchResult = searchResult
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
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
                    (case model.deck of
                        Success deck ->
                            viewDeck deck model

                        Loading ->
                            [ E.text "Chargement en cours..." ]

                        Failure _ ->
                            [ viewError model ]

                        NotAsked ->
                            [ E.none ]
                    )
                )
        ]
    }


viewDeck : Deck -> Model -> List (E.Element Msg)
viewDeck deck model =
    [ viewDeckTitle deck
    , viewHero deck
    , viewAffinities deck
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


viewDeckTitle : Deck -> E.Element Msg
viewDeckTitle deck =
    E.el
        [ Font.size 20
        , Font.bold
        ]
    <|
        E.text deck.title


viewHero : Deck -> E.Element Msg
viewHero deck =
    E.el
        [ Font.size 16
        , Font.bold
        ]
    <|
        E.text deck.hero.name


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


viewAffinities : Deck -> E.Element msg
viewAffinities deck =
    deck.affinities
        |> List.map (Faction.toString >> E.text)
        |> E.column []
