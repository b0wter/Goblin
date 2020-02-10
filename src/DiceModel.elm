module DiceModel exposing ( DiceModel
                          , addRoll, withAddedRoll
                          , withHistorySize, setHistorySize
                          , withHistoryDropState, setHistoryDropState
                          , clearHistory
                          , empty)

import Bootstrap.Dropdown as Dropdown
import Flip
import List.Extra as List

type alias DiceModel a =
    { rolls : List a 
    , lastRoll: Maybe a
    , maxHistory : Int
    , historyDropState : Dropdown.State
    }

empty : DiceModel a
empty = { rolls = [], lastRoll = Nothing, maxHistory = 4, historyDropState =  Dropdown.initialState }

addRoll : a -> DiceModel a -> DiceModel a
addRoll roll model =
    { model | rolls = List.addAndDrop model.maxHistory roll model.rolls, lastRoll = Just roll }

withAddedRoll : DiceModel a -> a -> DiceModel a
withAddedRoll =
    Flip.flip addRoll

setHistorySize : Int -> DiceModel a -> DiceModel a
setHistorySize newSize model = 
    { model | maxHistory = newSize, rolls = model.rolls |> List.limit (newSize + 1) }

withHistorySize : DiceModel a -> Int -> DiceModel a
withHistorySize =
    Flip.flip setHistorySize

setHistoryDropState : Dropdown.State -> DiceModel a -> DiceModel a
setHistoryDropState state model = { model | historyDropState = state }

withHistoryDropState : DiceModel a -> Dropdown.State -> DiceModel a
withHistoryDropState = Flip.flip setHistoryDropState

clearHistory : DiceModel a -> DiceModel a
clearHistory model = { model | rolls = [] }