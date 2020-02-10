module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser.Navigation as Navigation
import Browser exposing (UrlRequest)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, s, top)
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Button as Button
import Bootstrap.ListGroup as Listgroup
import Bootstrap.Modal as Modal
import Bootstrap.Table as Table
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form.Checkbox as Checkbox
import Random

import Roll
import List.Extra as List
import DiceModel

type alias Flags =
    {}

type alias Model =
    { navKey : Navigation.Key
    , page : Page
    , navState : Navbar.State
    , modalVisibility : Modal.Visibility
    , singleDie : DiceModel.DiceModel Roll.Single
    , multiDice : DiceModel.DiceModel Roll.Multi
    }

type Page
    = Home
    | GettingStarted
    | Modules
    | NotFound


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChange
        }

init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate url { navKey = key
                          , navState = navState
                          , page = Home
                          , modalVisibility = Modal.hidden
                          , singleDie = DiceModel.empty
                          , multiDice = DiceModel.empty
                          }
    in
        ( model, Cmd.batch [ urlCmd, navCmd ] )


type Msg
    = UrlChange Url
    | ClickedLink UrlRequest
    | NavMsg Navbar.State
    | CloseModal
    | ShowModal
    --
    | NewSingleDieResult Roll.Single
    | RollSingleDie Int
    | SingleRollDropStateChange Dropdown.State
    | SingleRollNewValue Int
    | ClearSingleDieResults
    | SetSingleDieExplode Bool
    --
    | NewMultiDiceResult Roll.Multi
    | RollMultiDice Int Int
    | MultiRollDropStateChange Dropdown.State
    | MultiRollNewValue Int
    | ClearMultiDiceResults
    | SetMultiDiceExplode Bool
    --
    | NewMixedDiceResult Roll.Mixed
    | RollMixedDice (List (Int, Int))
    | MixedRollDropStateChange Dropdown.State
    | MixedRollNewValue Int
    | ClearMixedDiceResults
    | SetMixedDiceExplode Bool


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
        [ Navbar.subscriptions model.navState NavMsg
        , Dropdown.subscriptions model.singleDie.historyDropState SingleRollDropStateChange
        , Dropdown.subscriptions model.multiDice.historyDropState MultiRollDropStateChange
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink req ->
             case req of
                 Browser.Internal url ->
                     ( model, Navigation.pushUrl model.navKey <| Url.toString url )

                 Browser.External href ->
                     ( model, Navigation.load href )

        UrlChange url ->
            urlUpdate url model

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modalVisibility = Modal.hidden }
            , Cmd.none
            )

        ShowModal ->
            ( { model | modalVisibility = Modal.shown }
            , Cmd.none
            )

        ClearSingleDieResults ->
            ( { model | singleDie = model.singleDie |> DiceModel.clearHistory }
            , Cmd.none
            )

        ClearMultiDiceResults ->
            ( { model | multiDice = model.multiDice |> DiceModel.clearHistory }
            , Cmd.none
            )

        NewSingleDieResult result ->
            ( { model | singleDie = model.singleDie |> DiceModel.addRoll result }
            , Cmd.none                                            
            )

        NewMultiDiceResult result ->
            ( { model | multiDice = model.multiDice |> DiceModel.addRoll result }
            , Cmd.none
            )

        RollSingleDie faceCount ->
            ( model
            --, Random.generate NewSingleDieResult (Random.map (\n -> { die = faceCount, result = n}) (Roll.singleRandomGenerator faceCount))
            , Random.generate NewSingleDieResult (Random.map (\n -> { die = faceCount, result = n}) (createNewSingleDieResult model.singleDie.explodes faceCount 0))
            )

        RollMultiDice faceCount diceCount ->
            ( model
            , Random.generate NewMultiDiceResult (Random.map (\n -> { die = faceCount, result = n}) (createNewMultiDiceResult model.multiDice.explodes faceCount diceCount)) --Roll.multiRandomGenerator faceCount diceCount)) 
            )

        SingleRollDropStateChange new ->
            ( { model | singleDie = model.singleDie |> DiceModel.setHistoryDropState new }
            , Cmd.none )

        MultiRollDropStateChange new ->
            ( { model | multiDice = model.multiDice |> DiceModel.setHistoryDropState new }
            , Cmd.none )

        SingleRollNewValue new ->
            ( { model | singleDie = model.singleDie |> DiceModel.setHistorySize new }
            , Cmd.none )

        MultiRollNewValue new ->
            ( { model | multiDice = model.multiDice |> DiceModel.setHistorySize new }
            , Cmd.none )

        SetSingleDieExplode new ->
            ( { model | singleDie = new |> DiceModel.asExplode model.singleDie }
            , Cmd.none )

        SetMultiDiceExplode new ->
            ( { model | multiDice = new |> DiceModel.asExplode model.multiDice }
            , Cmd.none )

        NewMixedDiceResult new -> (model, Cmd.none)
        RollMixedDice result -> (model, Cmd.none)
        MixedRollDropStateChange state -> (model, Cmd.none)
        MixedRollNewValue new  -> (model, Cmd.none)
        ClearMixedDiceResults -> (model, Cmd.none)
        SetMixedDiceExplode new -> (model, Cmd.none)

urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case decode url of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            ( { model | page = route }, Cmd.none )


decode : Url -> Maybe Page
decode url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
    |> UrlParser.parse routeParser


routeParser : Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home top
        , UrlParser.map GettingStarted (UrlParser.s "getting-started")
        , UrlParser.map Modules (UrlParser.s "modules")
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Bootstrap"
    , body =
        [ div []
            [ menu model
            , mainContent model
            , modal model
            ]
        ]
    }



menu : Model -> Html Msg
menu model =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> Navbar.container
        |> Navbar.brand [ href "#" ] [ text "Elm Bootstrap" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#getting-started" ] [ text "Getting started" ]
            , Navbar.itemLink [ href "#modules" ] [ text "Modules" ]
            ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            Home ->
                pageHome model

            GettingStarted ->
                pageGettingStarted model

            Modules ->
                pageModules model

            NotFound ->
                pageNotFound


pageHome : Model -> List (Html Msg)
pageHome model =
    [ h1 [] [ text "Home" ]
    , Grid.row []
        [ Grid.col [ Col.xs12, Col.sm6, Col.md4 ]
            [
                diceCard model
            ]
        , Grid.col [ Col.xs12, Col.sm6, Col.md5, Col.lg4 ]
            [
                multiDiceCard model
            ]
        ]
    ]


pageGettingStarted : Model -> List (Html Msg)
pageGettingStarted _ =
    [ h2 [] [ text "Getting started" ]
    , Button.button
        [ Button.success
        , Button.large
        , Button.block
        , Button.attrs [ onClick ShowModal ]
        ]
        [ text "Click me" ]
    ]


pageModules : Model -> List (Html Msg)
pageModules _ =
    [ h1 [] [ text "Modules" ]
    , Listgroup.ul
        [ Listgroup.li [] [ text "Alert" ]
        , Listgroup.li [] [ text "Badge" ]
        , Listgroup.li [] [ text "Card" ]
        ]
    ]


pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "SOrry couldn't find that page"
    ]


modal : Model -> Html Msg
modal model =
    Modal.config CloseModal
        |> Modal.small
        |> Modal.h4 [] [ text "Getting started ?" ]
        |> Modal.body []
            [ Grid.containerFluid []
                [ Grid.row []
                    [ Grid.col
                        [ Col.xs6 ]
                        [ text "Col 1" ]
                    , Grid.col
                        [ Col.xs6 ]
                        [ text "Col 2" ]
                    ]
                ]
            ]
        |> Modal.view model.modalVisibility

createNewSingleDieResult : Bool -> Int -> Int -> Random.Generator Int
createNewSingleDieResult explode faceCount previous =
    Random.andThen (\n -> if (n == faceCount) && explode then createNewSingleDieResult explode faceCount (previous + n) else Random.constant (previous + n)) (Roll.singleRandomGenerator faceCount)

createNewMultiDiceResult : Bool -> Int -> Int -> Random.Generator (List Int)
createNewMultiDiceResult explode faceCount diceCount =
    Random.list diceCount (createNewSingleDieResult explode faceCount 0)

diceCard: Model -> Html Msg
diceCard model =
    let button = \n -> Grid.col 
                        [ Col.xs6, Col.md4, Col.lg3 ] 
                        [ Button.button 
                          [ Button.outlinePrimary, Button.small, Button.attrs [ onClick (RollSingleDie  n), class "dice-roll-button disable-dbl-tap-zoom" ] ] 
                          [ text ("d" ++ (n |> String.fromInt)) ] ] in
    Card.config [ Card.attrs [ Html.Attributes.class "mb-4" ]]
        |> Card.headerH4 [] [ text "Roll single die" ]
        |> Card.footer [] 
            [ span [ class "float-right"] 
                   [ Button.button [ Button.secondary, Button.small, Button.onClick ClearSingleDieResults ] [ text "Clear" ] ] 
            , span [ class "float-left"]
                   [ singleRollMaxElementsDropdown model 
                   , span [ class "text-muted ml-2" ] [ small [] [ text "History" ] ]
                   ]
            , span [ class "ml-2 float-left text-muted"]
                   [ explodeCheckbox "single-explode" model.singleDie.explodes SetSingleDieExplode] 
            ]
        |> Card.block [ Block.attrs [ class "text-center"] ]
            [ Block.custom <| Grid.row [] 
                [ button 4, button 6, button 8, button 10, button 12, button 20 ]
            , Block.custom <| Grid.row []
                [ Grid.col [ Col.attrs [ class "mt-3" ] ] 
                  [ model |> diceResultMsg ] 
                ]
            ]
        |> Card.view


diceResultMsg: Model -> Html Msg
diceResultMsg model =
    if model.singleDie.rolls |> List.isEmpty then
        Html.div [] [ text "No dice rolled."]
    else
        Html.div [] (model.singleDie.rolls |> List.indexedMap dieResultMsg) --  ] --|> List.foldl (++) "") ]

dieResultMsg: Int -> Roll.Single -> Html Msg
dieResultMsg i roll =
    Html.span [ class ("no-wrap " ++ if i == 0 then "text-primary" else "")]
    [ Html.span [] [ text "｢" ]
    , Html.span [ class ("font-italic " ++ if i /= 0 then "font-muted" else "") ] [ text ("d" ++ (roll.die |> String.fromInt) ++ ": ") ]  --text ("｢d" ++ (roll.die |> String.fromInt) ++ ": " ++ (roll.result |> String.fromInt) ++ "」")]
    , Html.span [ class "font-weight-bold"] [ text (roll.result |> String.fromInt) ]
    , Html.span [] [ text "」"]
    ]

multiDieButton : (Int -> Int -> Msg) -> Int -> Int -> Table.Cell Msg
multiDieButton command faceCount dieCount =
    Table.td [ Table.cellAttr ( class "die-button-table" ) ] [ Button.button [ Button.outlinePrimary, Button.small, Button.attrs [ onClick (command faceCount dieCount), class "disable-dbl-tap-zoom"] ] [ text (dieCount |> String.fromInt) ] ]

multiDieButtonRow : Int -> Table.Row Msg
multiDieButtonRow faceCount =
    let dieCounts = [ 2, 3, 4, 5, 6, 7, 8 ] in
    Table.tr [] 
    ( Table.td [ Table.cellAttr ( class "die-button-table" ) ] [ text ("d" ++ (faceCount |> String.fromInt)) ] ::
      (dieCounts |> List.map (\n -> multiDieButton RollMultiDice faceCount n) ) )
    

multiDiceTable: Html Msg
multiDiceTable = 
    Table.simpleTable
        ( Table.simpleThead
            [ Table.th [] [ text "#"] 
            , Table.th [ Table.cellAttr (colspan 8) ] [ text "dice count" ]
            ]
        , Table.tbody []
            [ multiDieButtonRow 4
            , multiDieButtonRow 6
            , multiDieButtonRow 8
            , multiDieButtonRow 10
            , multiDieButtonRow 12
            , multiDieButtonRow 20
            ]
        )

multiDiceCard: Model -> Html Msg
multiDiceCard model =
    Card.config [ Card.attrs [ Html.Attributes.class "mb-4" ] ]
        |> Card.headerH4 [] [ text "Roll multiple dice" ]
        |> Card.footer [] 
            [ span [ class "float-right"] 
                   [ Button.button [ Button.secondary, Button.small, Button.onClick ClearMultiDiceResults ] [ text "Clear" ] ] 
            , span [ class "float-left"]
                   [ multiRollMaxElementsDropdown model 
                   , span [ class "text-muted ml-2" ] [ small [] [ text "History" ] ]
                   ]
            , span [ class "ml-2 float-left text-muted"]
                   [ explodeCheckbox "multi-explode" model.multiDice.explodes SetMultiDiceExplode] 
            ]
        |> Card.block [ Block.attrs [ class "text-center"] ]
            [ Block.custom <| multiDiceTable
            , Block.custom <| Grid.row []
                [ Grid.col [ ] 
                  [ model |> multiDiceResultMsg ] 
                ]
            ]
        |> Card.view

multiDiceResultMsg: Model -> Html Msg
multiDiceResultMsg model =
    if model.multiDice.rolls |> List.isEmpty then
        Html.div [] [ text "No dice rolled." ]
    else
        Html.div [] (model.multiDice.rolls |> List.indexedMap multiDieResultMsg)

multiDieResultMsg: Int -> Roll.Multi -> Html Msg
multiDieResultMsg i rolls =
    Html.div [] 
    [ Html.span [ class ("no-wrap " ++ if i == 0 then "text-primary" else "")]
      [ Html.span [] [ text "｢" ]
      , Html.span [ class ("font-italic " ++ if i /= 0 then "font-muted" else "") ] [ text ("d" ++ (rolls.die |> String.fromInt) ++ ": ") ]  --text ("｢d" ++ (roll.die |> String.fromInt) ++ ": " ++ (roll.result |> String.fromInt) ++ "」")]
      , Html.span [ class "font-weight-bold"] [ text (rolls.result |> List.map String.fromInt |> String.join ",") ]
      , Html.span [] [ text "」"]
    ] ]

rollMaxElementsDropdown : Dropdown.State -> Int -> (Int -> Msg) -> (Dropdown.State -> Msg) -> Html Msg
rollMaxElementsDropdown dropDownState historySize msg dropDownStateMsg =
    Dropdown.dropdown 
        dropDownState
        { options = [] 
        , toggleMsg = dropDownStateMsg
        , toggleButton =
            Dropdown.toggle [ Button.primary, Button.small ] [ text (historySize |> String.fromInt) ]
        , items = 
            [ Dropdown.buttonItem [ onClick (1 |> msg) ] [ text "1" ]
            , Dropdown.buttonItem [ onClick (2 |> msg) ] [ text "2" ]
            , Dropdown.buttonItem [ onClick (4 |> msg) ] [ text "4" ]
            , Dropdown.buttonItem [ onClick (6 |> msg) ] [ text "6" ]
            ] 
        }

singleRollMaxElementsDropdown : Model -> Html Msg
singleRollMaxElementsDropdown model =
    rollMaxElementsDropdown model.singleDie.historyDropState model.singleDie.maxHistory SingleRollNewValue SingleRollDropStateChange

multiRollMaxElementsDropdown : Model -> Html Msg
multiRollMaxElementsDropdown model =
    rollMaxElementsDropdown model.multiDice.historyDropState model.multiDice.maxHistory MultiRollNewValue MultiRollDropStateChange

explodeCheckbox: String -> Bool -> (Bool -> Msg) -> Html Msg
explodeCheckbox id val cmd =
    Checkbox.advancedCustom [ Checkbox.id id, Checkbox.checked val, Checkbox.onCheck cmd ] (Checkbox.label [] [ small [] [ text "Explode"] ])