module Pages.NewDeck exposing (Model, Msg, page)

import Backend exposing (createDeckCmd)
import Button exposing (button)
import Card exposing (Card, viewCardsTable)
import Colors exposing (darkGreen, darkerGreen, white)
import Deck exposing (Deck)
import Element as E exposing (rgb, rgb255)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Faction exposing (Faction, basic)
import Gen.Params.NewPack exposing (Params)
import Gen.Route as Route
import Header
import Kind
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
    { affinities : List Faction
    , allCards : List Card
    , error : Maybe String
    , hero : Maybe Card
    , heroSearchResult : List Card
    , heroSearchText : String
    , request : Request.With Params
    , title : String
    }


init : Shared.Model -> Request.With Params -> ( Model, Cmd Msg )
init shared req =
    ( { affinities = []
      , allCards = shared.cards
      , error = Nothing
      , hero = Nothing
      , heroSearchResult = []
      , heroSearchText = ""
      , request = req
      , title = ""
      }
    , case shared.status of
        Shared.Loaded ->
            Cmd.none

        _ ->
            Request.replaceRoute Route.Home_ req
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



-- UPDATE


type Msg
    = BackendReturnedDeck (Result Kinto.Error Deck)
    | UserChangedPackTitle String
    | UserToggledAffinity Faction Bool
    | UserChangedHeroSearchText String
    | UserClickedUnselectedHero Card
    | UserClickedSelectedHero Card
    | UserClickedCreate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserChangedPackTitle title ->
            ( { model | title = title }
            , Cmd.none
            )

        UserToggledAffinity affinity checked ->
            let
                isSelected : Bool
                isSelected =
                    List.member affinity model.affinities

                affinities : List Faction
                affinities =
                    if not checked && isSelected then
                        List.Extra.remove affinity model.affinities

                    else if checked && not isSelected then
                        affinity
                            :: model.affinities

                    else
                        model.affinities
            in
            ( { model
                | affinities = affinities
              }
            , Cmd.none
            )

        UserClickedUnselectedHero card ->
            ( { model
                | heroSearchResult =
                    List.map (\c -> { c | isSelected = c == card }) model.heroSearchResult
              }
            , Cmd.none
            )

        UserClickedSelectedHero card ->
            ( { model
                | heroSearchResult =
                    updateIf (\c -> c == card) (\_ -> { card | isSelected = False }) model.heroSearchResult
              }
            , Cmd.none
            )

        UserChangedHeroSearchText searchText ->
            let
                searchResult : List Card
                searchResult =
                    if not (String.isEmpty searchText) then
                        List.filter
                            (\card ->
                                (card.kind == Kind.hero)
                                    && String.startsWith (String.toLower searchText) (String.toLower card.name)
                            )
                            model.allCards

                    else
                        []
            in
            ( { model
                | heroSearchText = searchText
                , heroSearchResult = searchResult
              }
            , Cmd.none
            )

        UserClickedCreate ->
            let
                selectedHeroes : List Card
                selectedHeroes =
                    List.filter .isSelected model.heroSearchResult
            in
            case selectedHeroes of
                heroCard :: [] ->
                    let
                        deck =
                            deckBaseForHero model heroCard
                    in
                    ( model, createDeckCmd deck BackendReturnedDeck )

                _ ->
                    ( model, Cmd.none )

        BackendReturnedDeck (Ok deck) ->
            let
                _ =
                    Debug.log "deck created, redirection in progress" deck
            in
            ( model
            , Request.replaceRoute (Route.Deck__Id_ { id = deck.id }) model.request
            )

        BackendReturnedDeck (Err kintoError) ->
            -- TODO display error
            ( { model | error = Just ("Deck creation failed: " ++ Kinto.errorToString kintoError) }
            , Cmd.none
            )


deckBaseForHero : Model -> Card -> Deck
deckBaseForHero model heroCard =
    let
        heroCards =
            List.filter
                (\c ->
                    (c.cardSetCode == heroCard.cardSetCode)
                        && (c.kind /= Kind.alter_ego)
                        && (c /= heroCard)
                )
                model.allCards
    in
    { id = ""
    , affinities = model.affinities
    , cards = []
    , hero = heroCard
    , heroCards = heroCards
    , title = model.title
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
            [ E.width E.fill, E.height E.fill, E.inFront (Header.view <| Just "Créer un deck") ]
          <|
            E.el
                [ E.paddingXY 20 80
                ]
                (E.column
                    [ Font.size 11
                    , E.spacing 20
                    , E.width E.shrink
                    ]
                    [ viewTitleInput model
                    , viewHeroSearch model
                    , viewCardsTable model.heroSearchResult
                        UserClickedUnselectedHero
                        UserClickedSelectedHero
                        { showCount = True
                        , action = Nothing
                        }
                    , viewAffinities model
                    , viewCreateButton
                    ]
                )
        ]
    }


viewTitleInput : Model -> E.Element Msg
viewTitleInput model =
    Input.text
        [ E.padding 6
        , E.width (E.px 300)
        ]
        { onChange = UserChangedPackTitle
        , text = model.title
        , placeholder = Just <| Input.placeholder [] (E.text "titre")
        , label = Input.labelAbove [] (E.text "Titre")
        }


viewAffinities : Model -> E.Element Msg
viewAffinities model =
    E.column
        [ E.spacing 10
        ]
    <|
        E.text "Affinités"
            :: List.map (viewAffinity model) Faction.allAffinities


viewAffinity : Model -> Faction -> E.Element Msg
viewAffinity model affinity =
    Input.checkbox [ Font.size 20 ]
        { onChange = UserToggledAffinity affinity
        , icon = Input.defaultCheckbox
        , checked = List.member affinity model.affinities
        , label =
            Input.labelRight
                [ Font.size 11
                ]
            <|
                E.text <|
                    Faction.toString affinity
        }


viewCreateButton : E.Element Msg
viewCreateButton =
    button Button.Primary "Créer" 100 14 UserClickedCreate


viewHeroSearch : Model -> E.Element Msg
viewHeroSearch model =
    Input.search
        [ E.padding 6
        , E.width (E.px 200)
        ]
        { onChange = UserChangedHeroSearchText
        , text = model.heroSearchText
        , placeholder = Just <| Input.placeholder [] (E.text "Nom du héros")
        , label = Input.labelAbove [] (E.text "Héros")
        }
