module Pages.NewPack exposing (Model, Msg, page)

import Affinity exposing (Affinity)
import Card exposing (Card, viewCardsTable)
import Element as E exposing (rgb)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Gen.Params.NewPack exposing (Params)
import Gen.Route as Route
import List.Extra exposing (updateIf)
import Page
import Request
import Shared
import Utils.Route
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
    { affinities : List Affinity
    , title : String
    , allCards : List Card
    , availableCards : List Card
    , hero : Maybe Card
    , heroSearchText : String
    , heroSearchResult : List Card
    , cardSearchResult : List Card
    , cardSearchText : String
    , selectedCards : List Card
    }


init : Shared.Model -> Request.With Params -> ( Model, Cmd Msg )
init shared req =
    ( { title = ""
      , affinities = []
      , allCards = shared.cards
      , availableCards = filter [] shared.cards
      , hero = Nothing
      , heroSearchText = ""
      , heroSearchResult = []
      , cardSearchResult = []
      , cardSearchText = ""
      , selectedCards = []
      }
    , case shared.status of
        Shared.Loaded ->
            Cmd.none

        _ ->
            Utils.Route.navigate req.key Route.Home_
    )


filter : List Affinity -> List Card -> List Card
filter affinities cards =
    let
        selected_factions : List String
        selected_factions =
            [ "basic" ]
                ++ List.map Affinity.name affinities
    in
    cards |> List.filter (\card -> List.member card.faction selected_factions)



-- UPDATE


type Msg
    = UserChangedPackTitle String
    | UserToggledAffinity Affinity Bool
    | UserChangedHeroSearchText String
    | UserChangedCardSearchText String
    | UserClickedUnselectedCard Card
    | UserClickedSelectedCard Card
    | UserClickedUnselectedHero Card
    | UserClickedSelectedHero Card
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserChangedPackTitle title ->
            ( { model | title = title }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )

        UserToggledAffinity affinity checked ->
            let
                isSelected : Bool
                isSelected =
                    List.member affinity model.affinities

                affinities : List Affinity
                affinities =
                    if not checked && isSelected then
                        List.Extra.remove affinity model.affinities

                    else if checked && not isSelected then
                        affinity :: model.affinities

                    else
                        model.affinities
            in
            ( { model
                | affinities = affinities
                , availableCards = filter affinities model.allCards
              }
                |> searchCard model.cardSearchText
            , Cmd.none
            )

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
                                (card.kind == "Héros")
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
                , E.centerY
                , E.width E.fill
                , E.padding 10
                ]
                (E.column
                    [ Font.size 11
                    , E.spacing 20
                    , E.width E.fill
                    ]
                    [ viewTitleInput model
                    , viewHeroSearch model
                    , viewCardsTable model.heroSearchResult UserClickedUnselectedHero UserClickedSelectedHero
                    , viewAffinities model
                    , viewCardSearch model
                    , viewCardsTable model.cardSearchResult UserClickedUnselectedCard UserClickedSelectedCard
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
            :: List.map (viewAffinity model) Affinity.all


viewAffinity : Model -> Affinity -> E.Element Msg
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
                    Affinity.toString affinity
        }


viewCreateButton : E.Element Msg
viewCreateButton =
    E.link
        [ Background.color (rgb (31 / 255) (199 / 255) (170 / 255))
        , Border.color (rgb 0 0.7 0)
        , Border.rounded 7
        , E.padding 10
        , Font.size 14
        , Font.color (rgb 1 1 1)
        , E.alignRight
        , E.width (E.px 100)
        , Font.center
        ]
        { url = ""
        , label = E.text "Créer"
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
