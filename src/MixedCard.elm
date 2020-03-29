module MixedCard exposing ( MixedCard
                          , setName, asName
                          , addDie
                          , removeDie
                          , firstEmptyCard
                          , setId, asId
                          , isComplete
                          , empty
                          , addRoll
                          , setExplodes
                          , setHistoryDropState
                          , setHistoryLength
                          )

import DiceModel
import Roll
import UUID exposing (UUID)
import Flip
import List.Extra as List
import Bootstrap.Dropdown as Dropdown

type alias MixedCard =
    { dice : DiceModel.DiceModel Roll.Mixed
    , name : String
    , dieFaces : List Int
    , id : UUID
    }

firstEmptyCard : MixedCard
firstEmptyCard = empty (UUID.forName "https://gutsman.de/newId" UUID.urlNamespace)

setName : String -> MixedCard -> MixedCard
setName name card =
    { card | name = name }

asName : MixedCard -> String -> MixedCard
asName = Flip.flip setName

addDie : Int -> MixedCard -> MixedCard
addDie face card =
    { card | dieFaces = face :: card.dieFaces }

removeDie : Int -> MixedCard -> MixedCard
removeDie index card = { card | dieFaces = card.dieFaces |> List.removeIndex index }

setId : UUID -> MixedCard -> MixedCard
setId id card =
    { card | id = id }

asId : MixedCard -> UUID -> MixedCard
asId = Flip.flip setId

isComplete : MixedCard -> Bool
isComplete card = not <| (card.dieFaces |> List.isEmpty) || (card.name |> String.isEmpty)

empty : UUID -> MixedCard
empty id = { dice = DiceModel.empty, name = "", dieFaces = [], id = id }

addRoll : Roll.Mixed -> MixedCard -> MixedCard
addRoll roll card = { card | dice = card.dice |> DiceModel.addRoll roll }

setExplodes : Bool -> MixedCard -> MixedCard
setExplodes explodes card =
    let 
        newDiceModel dice =
            dice |> DiceModel.setExplode explodes 
    in
        { card | dice = card.dice |> newDiceModel }

setHistoryDropState : Dropdown.State -> MixedCard -> MixedCard
setHistoryDropState state card =
    let
        newDiceModel dice = 
            dice |> DiceModel.setHistoryDropState state
    in
        { card | dice = card.dice |> newDiceModel }

setHistoryLength : Int -> MixedCard -> MixedCard
setHistoryLength length card =
    let
        newDiceModel dice = 
            dice |> DiceModel.setHistorySize length
    in
        { card | dice = card.dice |> newDiceModel }
