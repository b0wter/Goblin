module DiceModel exposing ( DiceModel
                          , NewDiceSet
                          , addRoll, withAddedRoll
                          , asHistorySize, setHistorySize
                          , asHistoryDropState, setHistoryDropState
                          , asExplode, setExplode
                          , toggleExplode
                          , clearHistory
                          , withName
                          , empty)

import Bootstrap.Dropdown as Dropdown
import Flip
import List.Extra as List

type alias DiceModel a =
    { rolls : List a 
    , lastRoll: Maybe a
    , maxHistory : Int
    , historyDropState : Dropdown.State
    , explodes: Bool
    , name: String
    }

type alias NewDiceSet = List Int

empty : DiceModel a
empty = withName "<unnamed>"

withName : String -> DiceModel a
withName name = { rolls = [], lastRoll = Nothing, maxHistory = 4, historyDropState =  Dropdown.initialState, explodes = False, name = name }

addRoll : a -> DiceModel a -> DiceModel a
addRoll roll model = { model | rolls = List.addAndDrop model.maxHistory roll model.rolls, lastRoll = Just roll }

withAddedRoll : DiceModel a -> a -> DiceModel a
withAddedRoll = Flip.flip addRoll

setHistorySize : Int -> DiceModel a -> DiceModel a
setHistorySize newSize model = { model | maxHistory = newSize, rolls = model.rolls |> List.limit (newSize + 1) }

asHistorySize : DiceModel a -> Int -> DiceModel a
asHistorySize = Flip.flip setHistorySize

setHistoryDropState : Dropdown.State -> DiceModel a -> DiceModel a
setHistoryDropState state model = { model | historyDropState = state }

asHistoryDropState : DiceModel a -> Dropdown.State -> DiceModel a
asHistoryDropState = Flip.flip setHistoryDropState

clearHistory : DiceModel a -> DiceModel a
clearHistory model = { model | rolls = [] }

setExplode : Bool -> DiceModel a -> DiceModel a
setExplode state model = { model | explodes = state }

asExplode : DiceModel a -> Bool -> DiceModel a
asExplode = Flip.flip setExplode

toggleExplode : DiceModel a -> DiceModel a
toggleExplode model = { model | explodes = not model.explodes }