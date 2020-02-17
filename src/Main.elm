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
            , Random.generate NewSingleDieResult (Random.map (\n -> { die = faceCount, result = n}) (singleDieGenerator model.singleDie.explodes faceCount 0))
            )

        RollMultiDice faceCount diceCount ->
            ( model
            , Random.generate NewMultiDiceResult (Random.map (\n -> { die = faceCount, result = n}) (multiDiceGenerator model.multiDice.explodes faceCount diceCount)) --Roll.multiRandomGenerator faceCount diceCount)) 
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
    [ Grid.row []
        [ Grid.col [ Col.xs12, Col.sm6, Col.md4 ]
            [
                singleDieCard model
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


{--
    Code necessary to render the single die and multi dice cards.
--}

{-| Creates the generator necessary to create a new random number for a single die.
The boolean argument tells the generator to roll an additional time if the max value was rolled.
The last argument is an aggregator for _exploding_ results. Set this to zero.

    rollSixSidedDie : Random.Generator Int
    rollSixSidedDie =
        singleDieGenerator False 6 0
-}
singleDieGenerator : Bool -> Int -> Int -> Random.Generator Int
singleDieGenerator explode faceCount previous =
    Random.andThen (\n -> if (n == faceCount) && explode then singleDieGenerator explode faceCount (previous + n) else Random.constant (previous + n)) (Roll.singleRandomGenerator faceCount)

{-| Creates the generator necessary to create a new list of random dice throws.
The boolean argument tells the generator to roll an additional time if the max value was rolled.

    rollTwoDice : Random.Generator Int
    rollTwoDice = 
        multiDiceGenerator False 6 2

The examples returns a generator that returns the sum of two dice with six faces.
-}
multiDiceGenerator : Bool -> Int -> Int -> Random.Generator (List Int)
multiDiceGenerator explode faceCount diceCount =
    Random.list diceCount (singleDieGenerator explode faceCount 0)

{-| Renders the single roll card element.
-}

diceCard: String -> (Model -> Html Msg) -> String -> Bool -> (Bool -> Msg) -> Msg -> Html Msg -> (Model -> Html Msg) -> Model -> Html Msg
diceCard header elementsDropDown rollType explodes setDieExplodeMsg clearResultsMsg buttons resultList model =
    Card.config [ Card.attrs [ Html.Attributes.class "mb-4" ]]
        |> Card.headerH4 [] [ text header ]
        |> Card.footer [] 
            [ div [ class "d-flex justify-content-between"] 
                [ div [ class ""]
                    [ elementsDropDown model 
                    , span [ class "text-muted ml-2" ] [ small [] [ text "History" ] ]
                    ]
                , div [ class "mb-auto mt-auto"]
                    [ explodeCheckbox (rollType ++ "-explode") explodes setDieExplodeMsg] 
                , div [ class ""] 
                    [ Button.button [ Button.secondary, Button.small, Button.onClick clearResultsMsg ] [ text "Clear" ] ] 
                ]
            ]
        |> Card.block [ Block.attrs [ class "text-center"] ]
            [ Block.custom <| buttons
            , Block.custom <| Grid.row []
                [ Grid.col [ ] 
                  [ model |> resultList ] 
                ]
            ]
        |> Card.view    

singleDieCard: Model -> Html Msg
singleDieCard model =
    let button = \n -> Grid.col 
                        [ Col.xs6, Col.md4, Col.lg3 ] 
                        [ Button.button 
                          [ Button.outlinePrimary, Button.small, Button.attrs [ onClick (RollSingleDie  n), class "dice-roll-button disable-dbl-tap-zoom" ] ] 
                          [ text ("d" ++ (n |> String.fromInt)) ] ] in
    let buttons =
         Grid.row [] [ button 4, button 6, button 8, button 10, button 12, button 20 ]
    in 
        diceCard "Roll single die" singleRollMaxElementsDropdown "single-die" model.singleDie.explodes SetSingleDieExplode ClearSingleDieResults buttons singleDieResultList model

multiDiceCard: Model -> Html Msg
multiDiceCard model =
    diceCard "Roll multiple dice" multiRollMaxElementsDropdown "multi-dice" model.multiDice.explodes SetMultiDiceExplode ClearMultiDiceResults multiDiceTable multiDiceResultList model
{- ----------------------------------------------------------------- -}

{- Renders a list of results. -}
diceResultList: List a -> (Int -> a -> Html Msg) -> Html Msg
diceResultList rolls elementRenderer =
    if rolls |> List.isEmpty then
        Html.div [] [ text "No dice rolled." ]
    else
        Html.div [] (rolls |> List.indexedMap elementRenderer)

multiDiceResultList: Model -> Html Msg
multiDiceResultList model =
    diceResultList model.multiDice.rolls multiDieResult

singleDieResultList: Model -> Html Msg
singleDieResultList model =
    diceResultList model.singleDie.rolls singleDieResult
{- ----------------------------------------------------------------- -}

{- Renders the results of single and multi dice rolls. -}
dieResult: (result -> Int) -> (result -> String) -> Int -> result -> Html Msg
dieResult asDie asRolls i result =
    Html.span [ class ("no-wrap " ++ if i == 0 then "text-primary" else "")]
    [ Html.span [] [ text "｢" ]
    , Html.span [ class ("font-italic " ++ if i /= 0 then "font-muted" else "") ] [ text ("d" ++ (result |> asDie |> String.fromInt) ++ ": ") ] 
    , Html.span [ class "font-weight-bold"] [ text (result |> asRolls) ]
    , Html.span [] [ text "」"]
    ]

singleDieResult: Int -> Roll.Single -> Html Msg
singleDieResult i roll = 
    dieResult (\r -> r.die) (\r -> r.result |> String.fromInt) i roll

multiDieResult: Int -> Roll.Multi -> Html Msg
multiDieResult i roll =
    dieResult (\r -> r.die) (\r -> r.result |> List.map String.fromInt |> String.join ", ") i roll
{- ----------------------------------------------------------------- -}

{- Required helpers to render the table for all multi-dice buttons. -}
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
{- ----------------------------------------------------------------- -}


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
    Checkbox.advancedCustom [ Checkbox.id id, Checkbox.checked val, Checkbox.onCheck cmd ] (Checkbox.label [] [ small [ class "text-muted" ] [ text "Explode"] ])