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
import Bootstrap.Modal as Modal
import Bootstrap.ListGroup as Listgroup
import Bootstrap.Table as Table
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Accordion as Accordion
import Random
import UUID exposing (UUID)
import Json.Decode as Decode

import Ports
import Roll
import List.Extra as List
import DiceModel
import MixedCard
import DebugOutput


-- MAIN

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
        




-- MODEL ----------------------------------------------------------------------------------------------------------------

type alias Flags =
    {
        serializedMixedCards : Decode.Value
    }

type alias Model =
    { navKey : Navigation.Key
    , page : Page
    , navState : Navbar.State
    , modelNewMixedCardVisibility : Modal.Visibility
    , singleDie : DiceModel.DiceModel Roll.Single
    , multiDice : DiceModel.DiceModel Roll.Multi
    , mixedCards : List MixedCard.MixedCard
    , newMixedCard : MixedCard.MixedCard
    , storageTestData : Maybe String
    , debugMessages : List DebugOutput.Message
    , instructionsToggleState : Accordion.State
    }

type Page
    = Home
    | Instructions
    | Modules
    | NotFound



init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( serializedMixedCardError, serializedMixedCards ) = 
            case flags.serializedMixedCards |> MixedCard.decodeMultiple of
                Ok cards -> (Nothing, cards)
                Err e -> (Just e, [])

        ( model, urlCmd ) =
            urlUpdate url { navKey = key
                          , navState = navState
                          , page = Home
                          , modelNewMixedCardVisibility = Modal.hidden
                          , singleDie = DiceModel.withName "Roll single die"
                          , multiDice = DiceModel.withName "Roll multiple dice"
                          , mixedCards = serializedMixedCards
                          , newMixedCard = MixedCard.firstEmptyCard
                          , storageTestData = Just (serializedMixedCardError |> Maybe.map Decode.errorToString |> Maybe.withDefault "")
                          , debugMessages = []
                          , instructionsToggleState = Accordion.initialState
                          }
    in
        ( model, Cmd.batch [ urlCmd, navCmd, Random.generate ResetNewMixedCard UUID.generator ] )





-- UPDATE ----------------------------------------------------------------------------------------------------------------

type Msg
    = UrlChange Url
    | ClickedLink UrlRequest
    | NavMsg Navbar.State
    | CloseModal
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
    | NewMixedCardResult (UUID, Roll.Mixed)
    | RollMixedCard (UUID, List Int, Bool)
    | MixedCardDropStateChange (UUID, Dropdown.State)
    | MixedCardNewValue (UUID, Int)
    | ClearMixedCardResults UUID
    | SetMixedCardExplode (UUID, Bool)
    | ResetNewMixedCard UUID
    | DeleteMixedCard UUID
    --
    -- These messages are used for the creation of a new MixedCard.
    | ClearNewSet
    | AddNewSet
    | AddNewDieToSet Int
    | RemoveDieFromNewSet Int
    | NewDieSetNameChanged String
    | ShowMixedSetModal
    --
    -- Ports related messages
    | StoreData Ports.StorageObject
    | RetrievedData Ports.StorageObject
    | RequestRetrieval String
    --
    -- Messages not fitting into other categories
    | ToggleInstructions Accordion.State
    | ToggleTheme

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
            ( List.append
                [ Navbar.subscriptions model.navState NavMsg
                , Accordion.subscriptions model.instructionsToggleState ToggleInstructions
                , Ports.retrieve (\data -> RetrievedData data)
                , Dropdown.subscriptions model.singleDie.historyDropState SingleRollDropStateChange
                , Dropdown.subscriptions model.multiDice.historyDropState MultiRollDropStateChange ]
                (model.mixedCards |> List.map (\x -> Dropdown.subscriptions x.dice.historyDropState (\z -> MixedCardDropStateChange (x.id, z))))
            )


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
            ( { model | modelNewMixedCardVisibility = Modal.hidden }
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
            , Random.generate NewSingleDieResult (Random.map (\n -> { die = faceCount, result = n}) (singleDieGenerator model.singleDie.explodes 0 faceCount))
            )

        RollMultiDice faceCount diceCount ->
            ( model
            , Random.generate NewMultiDiceResult (Random.map (\n -> { die = faceCount, result = n}) (multiDiceGenerator model.multiDice.explodes faceCount diceCount)) 
            )

        RollMixedCard (id, dice, explode) -> 
            ( model
            , Random.generate NewMixedCardResult (rollMixedSet id explode dice) 
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

        NewMixedCardResult (id, result) -> 
            ( let
                card = model.mixedCards |> List.find (\x -> x.id == id)
              in
                case card of
                    Just c ->
                        let
                            updatedCard =
                                c |> MixedCard.addRoll result  
                        in
                            { model | mixedCards = model.mixedCards |> List.replaceBy (\x -> x.id == id) updatedCard }
                    Nothing ->
                        model
            , Cmd.none
            )

        MixedCardDropStateChange (id, state) -> 
            ( model |> setForMixedSet (\c -> MixedCard.setHistoryDropState state c) id 
            , Cmd.none)

        MixedCardNewValue (id, length) -> 
            let 
                newModel = model |> setForMixedSet (\c -> MixedCard.setHistoryLength length c) id
            in
                update (saveMixedCardsToLocalStorage newModel) newModel

        ClearMixedCardResults id -> 
            ( model |> setForMixedSet (\c -> c |> MixedCard.clearHistory) id
            , Cmd.none)

        SetMixedCardExplode (id, checked) -> 
            let
                newModel = model |> setForMixedSet (\c -> MixedCard.setExplodes checked c) id
            in
                update (saveMixedCardsToLocalStorage newModel) newModel

        DeleteMixedCard id ->
            removeMixedCard id model

        ResetNewMixedCard id ->
            ( { model | newMixedCard = MixedCard.empty id }
            , Cmd.none)

        ShowMixedSetModal ->
            ( { model | modelNewMixedCardVisibility = Modal.shown }
            , Cmd.none
            )

        ClearNewSet -> 
            ( { model | newMixedCard = MixedCard.empty model.newMixedCard.id }
            , Cmd.none)

        AddNewSet -> 
            model |> addNewSet

        AddNewDieToSet d -> 
            ( { model | newMixedCard = model.newMixedCard |> MixedCard.addDie d }
            , Cmd.none)
        
        RemoveDieFromNewSet index -> 
            ( { model | newMixedCard = model.newMixedCard |> MixedCard.removeDie index }
            , Cmd.none)
        
        NewDieSetNameChanged name ->
            ( { model | newMixedCard = model.newMixedCard |> MixedCard.setName name }
            , Cmd.none)

        StoreData data ->
            ( model
            , Ports.store data)

        RetrievedData data ->
            ( { model | storageTestData = Just data.value }
            , Cmd.none)

        RequestRetrieval key ->
            ( model
            , Ports.requestRetrieval key)

        ToggleInstructions state ->
            ( {model | instructionsToggleState = state }
            , Cmd.none)

        ToggleTheme ->
            ( model
            , Ports.toggleTheme () )

addNewSet : Model -> (Model, Cmd Msg)
addNewSet model =
    let 
        isComplete = 
            model.newMixedCard |> MixedCard.isComplete 

        -- Update the model to include the new mixed set. 
        modelWithNewSet = 
            if isComplete then 
                let addedSet = model |> addMixedSet model.newMixedCard
                in { addedSet | modelNewMixedCardVisibility = Modal.hidden }
            else 
                model

        {- 
        Create the save command if the mixed set is complete.
        `newModel` should be equal to `modelWithSet` but  we will use `newModel`
        just in case the save command will do something extravagant in the future. 
        -}
        (newModel, newCmd) = 
            if isComplete then 
                modelWithNewSet |> update (saveMixedCardsToLocalStorage modelWithNewSet) 
            else 
                (modelWithNewSet, Cmd.none) 

        -- In case a new card was created we need to generate a new UUID.
        -- This will also reset the temporary fields.
        additionalCommand =
            if isComplete then
                Random.generate ResetNewMixedCard UUID.generator
            else
                Cmd.none
    in 
        (newModel, Cmd.batch [ newCmd, additionalCommand ])
            
removeMixedCard : UUID -> Model -> (Model, Cmd Msg)
removeMixedCard id model =
    let
        newModel =
            { model | mixedCards = model.mixedCards |> List.filter (\x -> x.id /= id) }
    in
        newModel |> update (saveMixedCardsToLocalStorage newModel)

setForMixedSet : (MixedCard.MixedCard -> MixedCard.MixedCard) -> UUID -> Model -> Model
setForMixedSet f id model =
    let 
        updatedCard =
            model.mixedCards |> List.find (\c -> c.id == id) |> Maybe.map f
    in
        case updatedCard of
            Nothing -> model
            Just card -> { model | mixedCards = model.mixedCards |> List.replaceBy (\c -> c.id == id) card }


addMixedSet : MixedCard.MixedCard -> Model -> Model
addMixedSet card model =
    { model | mixedCards = card :: model.mixedCards }

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
        , UrlParser.map Instructions (UrlParser.s "instructions")
        , UrlParser.map Modules (UrlParser.s "modules")
        ]


saveMixedCardsToLocalStorage : Model -> Msg
saveMixedCardsToLocalStorage model =
    StoreData (model.mixedCards |> MixedCard.encodeMultiple |> Ports.createStorageObject "serializedMixedCards")

{--
    Code necessary to render the single die and multi die cards.
--}

{-| Creates the generator necessary to create a new random number for a single die.
The boolean argument tells the generator to roll an additional time if the max value was rolled.
The last argument is an aggregator for _exploding_ results. Set this to zero.

    rollSixSidedDie : Random.Generator Int
    rollSixSidedDie =
        singleDieGenerator False 6 0
-}
singleDieGenerator : Bool -> Int -> Int -> Random.Generator Int
singleDieGenerator explode previous faceCount =
    Random.andThen (\n -> if (n == faceCount) && explode then singleDieGenerator explode faceCount (previous + n) else Random.constant (previous + n)) (Roll.singleRandomGenerator faceCount)

{-| Creates the generator necessary to create a new list of random dice throws.
The boolean argument tells the generator to roll an additional time if the max value was rolled.

    rollTwoDice : Random.Generator Int
    rollTwoDice = 
        multiDiceGenerator False 6 2

The examples returns a generator that returns the sum of two dice with six faces.

(This is merely a convenience method.)
-}
multiDiceGenerator : Bool -> Int -> Int -> Random.Generator (List Int)
multiDiceGenerator explode faceCount diceCount =
    Random.list diceCount (singleDieGenerator explode 0 faceCount)

{-| Transforms a list of generators into a single generator that produces a list. 
This is limited to generators that produce the same type.
-}
mixedRandomGenerator : List (Random.Generator a) -> Random.Generator (List a)
mixedRandomGenerator generators =
    let 
        step remaining accumulator = 
            case remaining of
                [] -> Random.constant accumulator
                head :: tail -> Random.andThen (\n -> step tail (n :: accumulator)) head 
    in
        step generators []

mixedDiceGenerator : Bool -> List Int -> Random.Generator (List Int)
mixedDiceGenerator explode dice =
    dice |> List.map (singleDieGenerator explode 0) |> mixedRandomGenerator

rollMixedSet : UUID -> Bool -> List Int -> Random.Generator (UUID, Roll.Mixed)
rollMixedSet id explode dice =
    let 
        generator = mixedDiceGenerator explode dice -- Random.Generator #List Int#
    in
        Random.map (\results -> (id, results |> List.map2 (\x y -> { die = x, result = y }) dice)) generator





-- VIEW -------------------------------------------------------------------------------------

view : Model -> Browser.Document Msg
view model =
    { title = "Goblin - Pen & Paper Tools"
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
        |> Navbar.fixTop
        |> Navbar.primary
        |> Navbar.brand [ href "#" ] [ img [ src "static/images/icons.png", class "brand-image" ] [text "Home"] ]
        |> Navbar.items
            [ Navbar.itemLink [ class "font-weight-bold", href "#", onClick ShowMixedSetModal ] [ text "Create mixed set"]
            , Navbar.itemLink [ class "", href "#instructions" ] [ text "Instructions" ]
            , Navbar.itemLink [ class "", href "#", onClick ToggleTheme ] [ text "Switch theme"]
            ]
        |> Navbar.view model.navState

mainContent : Model -> Html Msg
mainContent model =
    div [ class "large-top-margin" ] [
    Grid.container [ ] <|
        case model.page of
            Home ->
                pageHome model

            Instructions ->
                pageInstructions model

            Modules ->
                pageModules model

            NotFound ->
                pageNotFound
    ]


pageHome : Model -> List (Html Msg)
pageHome model =
    [ Grid.row []
        [ Grid.col [ Col.xs, Col.attrs [ class "d-flex" ] ]
            [
                --Button.button [ Button.light, Button.small, Button.attrs [ onClick ToggleTheme, class "mb-3 ml-1", id "theme-button" ] ] [ text "Theme" ]
                {- Put debug elements here :)
                [ Button.button [ Button.primary, Button.small, Button.onClick (StoreData (model.mixedDice |> MixedCard.encodeMultiple |> Ports.createStorageObject "serializedMixedCards")) ] [ text "Add" ] 
                , Button.button [ Button.primary, Button.small, Button.onClick (RequestRetrieval "serializedMixedCards") ] [ text "Get" ] 
                , div [] [ text (model.storageTestData |> Maybe.withDefault "<>") ]
                ]
                -}
            ]
        ]
    , Grid.row []
        [ Grid.col [ Col.xs12, Col.sm6, Col.md6, Col.lg4 ]
            [
                singleDieCard model
            ]
        , Grid.col [ Col.xs12, Col.sm6, Col.md6, Col.lg8 ]
            [
                multiDiceCard model
            ]
        , Grid.col [ Col.xs12 ]
            [
                model |> mixedSetCards
            ]
        ]
    , Grid.row []
        [ Grid.col [ Col.xs12 ] (model.debugMessages |> List.map DebugOutput.messageAsAlert)
        ]
    ]


pageInstructions : Model -> List (Html Msg)
pageInstructions _ =
    [ p [ class "mt-3" ] [ span [ class "font-weight-bold" ] [ text "Custom dice sets"  ], span [ Spacing.ml1Sm ] [ text " - you can create custom sets of mixed dice. To do so you have to enter a name for the set in the 'Create new set' box and use the buttons below to add dice to the set. Afterwards click the 'Add' button and a new box is created. Customs sets are persisted between visits to this site. This happens automatically. You can use the 'X' button in the top right corner of a card to delete a custom set."] ]
    , p [] [ span [ class "font-weight-bold" ] [ text "Explode"  ], span [ Spacing.ml1Sm ] [ text " - you can set the option for dice to 'explode'. This will cause a die to be rolled again if its maximum face count has been rolled (can trigger multiple times for a single row)."] ]
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
        |> Modal.h5 [] [ text "Create custom set" ]
        |> Modal.body []
            [ createMixedSetCard model ] 
        |> Modal.view model.modelNewMixedCardVisibility
    

{- Creates cards for single and multi-dice rolls.
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

{-| Renders the single roll card element.
-}
singleDieCard: Model -> Html Msg
singleDieCard model =
    let button = \n -> Grid.col 
                        [ Col.xs6, Col.md4, Col.lg3 ] 
                        [ Button.button 
                          [ Button.outlinePrimary, Button.small, Button.attrs [ onClick (RollSingleDie  n), class "dice-roll-button disable-dbl-tap-zoom" ] ] 
                          [ text ("d" ++ (n |> String.fromInt)) ] ] in
    let buttons =
         Grid.row [] (Roll.dieTypes |> List.map button)
    in 
        diceCard model.singleDie.name singleRollMaxElementsDropdown "single-die" model.singleDie.explodes SetSingleDieExplode ClearSingleDieResults buttons singleDieResultList model

{-| Renders the multi-dice roll card element.
-}
multiDiceCard: Model -> Html Msg
multiDiceCard model =
    diceCard model.multiDice.name multiRollMaxElementsDropdown "multi-dice" model.multiDice.explodes SetMultiDiceExplode ClearMultiDiceResults multiDiceTable multiDiceResultList model

createMixedSetCard: Model -> Html Msg
createMixedSetCard model =
    let 
        createAddDieButton faceCount = 
            Button.button [ Button.attrs [ class "mt-2" ], Button.outlinePrimary, Button.small, Button.onClick (AddNewDieToSet  faceCount) ] [ text ("d" ++ (faceCount |> String.fromInt)) ] 
    in
        div []
        [ Form.group [] [ Input.text [ Input.id "dice-set-name", Input.onInput NewDieSetNameChanged, Input.value model.newMixedCard.name, Input.attrs [ placeholder "Name" ] ] ]
        , div [] [ text "Add die" ]
        , Form.group [ Form.attrs [ class "d-flex justify-content-between flex-wrap" ] ] (Roll.dieTypes |> List.map createAddDieButton)
        , div [] [ text "Current set"], model |> newDiceSetList
        , div [ class "d-flex flex-row-reverse" ] 
            [ div [ class "" ]
                [ Button.button [ Button.primary, Button.small, Button.onClick AddNewSet ] [ text "Add" ] 
                , Button.button [ Button.secondary, Button.small, Button.onClick ClearNewSet, Button.attrs [ class "ml-3" ] ] [ text "Clear" ] 
                ]
            ]
        ]


mixedSetCards: Model -> Html Msg
mixedSetCards model =
    let 
        regularSize =  [ Col.xs12, Col.sm6, Col.md6, Col.lg4 ]

        largeSize = [ Col.xs12, Col.sm12, Col.md12, Col.lg6 ]

        extraLargeSize = [ Col.xs12, Col.sm12, Col.md12, Col.lg12 ]

        diceInCard c = c.dieFaces |> List.length

        makeColumn diceCount card = 
            if diceCount > 8 then
                Grid.col extraLargeSize [ card ] 
            else if diceCount > 6 then
                Grid.col largeSize [ card ] 
            else
                Grid.col regularSize [ card ] 
    in
        Grid.row [] (model.mixedCards |> List.map (\c -> c |> (mixedSetCard >> makeColumn (c |> diceInCard))))

mixedSetCard : MixedCard.MixedCard -> Html Msg
mixedSetCard card =
    let
        dieTable =
            let 
                resultRow i r =
                    Table.tr (if i == 0 then [ Table.rowAttr (class "text-primary font-weight-bold") ] else []) (r |> List.map (\x -> Table.td [] [ text (x.result |> String.fromInt)]))
            in
                Table.simpleTable
                    ( Table.simpleThead
                        (card.dieFaces |> List.map (\f -> Table.th [] [ text ("d" ++ (f |> String.fromInt))]))
                    , Table.tbody [] (if card.dice.rolls |> List.isEmpty then [ Table.tr [] [ Table.td [ Table.cellAttr (colspan (card.dieFaces |> List.length)) ] [ text "No dice rolled."] ] ] else card.dice.rolls |> List.indexedMap resultRow)
                    )
    in
        Card.config [ Card.attrs [ Html.Attributes.class "mb-4" ]]
            |> Card.headerH4 [] [ div [ class "d-flex justify-content-between"] [ div [] [ text card.name ], div [] [ small [ class "cursor-pointer", onClick (DeleteMixedCard card.id) ] [ text "❌" ] ] ] ]
            |> Card.footer []
                [ div [ class "d-flex justify-content-between" ] 
                    [ div [ class ""]
                        [ mixedRollMaxElementsDropDown card 
                        , span [ class "text-muted ml-2" ] [ small [] [ text "History" ] ]
                        ]
                    , div [ class "mb-auto mt-auto"]
                        [ explodeCheckbox ((card.id |> UUID.toString) ++ "-explode") card.dice.explodes (\b -> SetMixedCardExplode (card.id, b))] 
                    , div [ class ""] 
                        [ Button.button [ Button.secondary, Button.small, Button.onClick (ClearMixedCardResults card.id) ] [ text "Clear" ] ] 
                    ]
                ]         
            |> Card.block [ Block.attrs [ class "text-center pb-0"] ]
                [ Block.custom <| div [ class "mb-3" ] [ Button.button [ Button.attrs [ class "w-100" ], Button.primary, Button.small, Button.onClick (RollMixedCard (card.id, card.dieFaces, card.dice.explodes)) ] [ text "Roll" ] ]
                , Block.custom <| dieTable
                ]
            |> Card.view

newDiceSetList: Model -> Html Msg
newDiceSetList model =
    let dieButton i d =
         Button.button [ Button.secondary, Button.small, Button.onClick (RemoveDieFromNewSet i), Button.attrs [ Spacing.mb1, Spacing.mr1 ] ] [ text ("d" ++ (d |> String.fromInt)), text " ❌￼"]
    in
        if (model.newMixedCard.dieFaces |> List.length) == 0 then
            div [ Spacing.mb1, Spacing.mt1 ] [ text "Add dice using the buttons above." ]
        else
            div [] (model.newMixedCard.dieFaces |> List.indexedMap dieButton)

{- ----------------------------------------------------------------- -}

{- Renders a list of results. -}
diceResultList: List a -> (Int -> a -> Html Msg) -> Html Msg
diceResultList rolls elementRenderer =
    if rolls |> List.isEmpty then
        Html.div [ Spacing.mt3 ] [ text "No dice rolled." ]
    else
        Html.div [ Spacing.mt3, class "d-flex flex-wrap justify-content-center" ] (rolls |> List.indexedMap elementRenderer)

multiDiceResultList: Model -> Html Msg
multiDiceResultList model =
    diceResultList model.multiDice.rolls multiDieResult

singleDieResultList: Model -> Html Msg
singleDieResultList model =
    diceResultList model.singleDie.rolls singleDieResult

mixedDieResultList: MixedCard.MixedCard -> Html Msg
mixedDieResultList card =
    diceResultList card.dice.rolls mixedDieResult
{- ----------------------------------------------------------------- -}

{- Renders the results of single and multi dice rolls. -}
dieResult: (result -> Int) -> (result -> String) -> Int -> result -> Html Msg
dieResult asDie asRolls i result =
    Html.div [ class ("no-wrap " ++ if i == 0 then "text-primary" else "")]
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

mixedDieResult: Int -> Roll.Mixed -> Html Msg
mixedDieResult i roll =
    dieResult (\r -> r |> List.head |> Maybe.map (\x -> x.die) |> Maybe.withDefault 0) (\r -> r |> List.map (\x -> x.result |> String.fromInt) |> String.join "| ") i roll
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
    Table.table
        { options =  [ Table.attr Spacing.mb0 ]
        , thead = Table.simpleThead
            [ Table.th [] [ text "#"] 
            , Table.th [ Table.cellAttr (colspan 8) ] [ text "dice count" ]
            ]
        , tbody = Table.tbody [] (Roll.dieTypes |> List.map multiDieButtonRow)
        }
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

mixedRollMaxElementsDropDown : MixedCard.MixedCard -> Html Msg
mixedRollMaxElementsDropDown card =
    rollMaxElementsDropdown card.dice.historyDropState card.dice.maxHistory (\x -> MixedCardNewValue (card.id, x)) (\x -> MixedCardDropStateChange (card.id, x))

explodeCheckbox: String -> Bool -> (Bool -> Msg) -> Html Msg
explodeCheckbox id val cmd =
    Checkbox.advancedCustom [ Checkbox.id id, Checkbox.checked val, Checkbox.onCheck cmd ] (Checkbox.label [] [ small [ class "text-muted" ] [ text "Explode"] ])

addDebugMessage : DebugOutput.Message -> Model -> Model
addDebugMessage text model =
    { model | debugMessages = text :: model.debugMessages }
