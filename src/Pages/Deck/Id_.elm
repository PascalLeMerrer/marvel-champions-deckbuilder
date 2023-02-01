module Pages.Deck.Id_ exposing (Model, Msg, page)

import Card exposing (Card, viewCardsTable)
import Element as E exposing (rgb)
import Element.Font as Font
import Element.Input as Input
import Faction exposing (Faction, basic)
import Gen.Params.Deck.Id_ exposing (Params)
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
    , selectedCards : List Card
    }


init : Shared.Model -> Request.With Params -> ( Model, Cmd Msg )
init shared req =
    ( { allCards = shared.cards
      , availableCards = filter [] shared.cards
      , cardSearchResult = []
      , cardSearchText = ""
      , selectedCards = []
      }
    , -- TODO load deck
      Cmd.none
    )



-- UPDATE


type Msg
    = UserChangedCardSearchText String
    | UserClickedUnselectedCard Card
    | UserClickedSelectedCard Card


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
                    List.map (\c -> { c | isSelected = c == card }) model.cardSearchResult
              }
            , Cmd.none
            )

        UserClickedSelectedCard card ->
            ( { model
                | cardSearchResult =
                    updateIf (\c -> c == card) (\_ -> { card | isSelected = False }) model.cardSearchResult
              }
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
            [ E.width E.fill ]
          <|
            E.el
                [ E.centerX
                , E.width E.fill
                , E.padding 20
                ]
                (E.column
                    [ Font.size 11
                    , E.spacing 20
                    , E.width E.fill
                    ]
                    [ viewCardSearch model
                    , viewCardsTable model.cardSearchResult
                        UserClickedUnselectedCard
                        UserClickedSelectedCard
                        { showCount = True
                        , action = Nothing
                        }
                    ]
                )
        ]
    }


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
