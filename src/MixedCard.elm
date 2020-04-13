module MixedCard exposing ( MixedCard
                          , setName, asName
                          , addDie
                          , removeDie
                          , firstEmptyCard
                          , setId, asId
                          , isComplete
                          , empty
                          , addRoll
                          , mapDiceModel
                          , clearHistory
                          , setExplodes
                          , setHistoryDropState
                          , setHistoryLength
                          , encodeMultiple
                          , decodeMultiple
                          , decodeMultipleFromString
                          )

import DiceModel
import Roll
import UUID exposing (UUID)
import Flip
import List.Extra as List
import Bootstrap.Dropdown as Dropdown
import Json.Encode as Encode
import Json.Decode as Decode
import Maybe.Extra exposing(values)

type alias MixedCard =
    { dice : DiceModel.DiceModel Roll.Mixed
    , name : String
    , dieFaces : List Int
    , id : UUID
    }

type alias JsonModel =
    { name : String
    , dieFaces : List Int
    , id : String
    , explodes : Bool
    , maxHistory : Int
    }

createWithoutHistory : String -> (List Int) -> Bool -> Int -> UUID -> MixedCard
createWithoutHistory name dieFaces explodes maxHistory id =
    {
        dice = DiceModel.empty |> DiceModel.setExplode explodes |> DiceModel.setHistorySize maxHistory,
        name = name,
        dieFaces = dieFaces,
        id = id
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

mapDiceModel : (DiceModel.DiceModel Roll.Mixed -> DiceModel.DiceModel Roll.Mixed) -> MixedCard -> MixedCard
mapDiceModel f card =
    let 
        newDiceModel dice =
            dice |> f
    in
        { card | dice = card.dice |> newDiceModel }

setExplodes : Bool -> MixedCard -> MixedCard
setExplodes explodes card =
    mapDiceModel (\d -> d |> DiceModel.setExplode explodes) card

setHistoryDropState : Dropdown.State -> MixedCard -> MixedCard
setHistoryDropState state card =
    mapDiceModel (\d -> d |> DiceModel.setHistoryDropState state) card

setHistoryLength : Int -> MixedCard -> MixedCard
setHistoryLength length card =
    mapDiceModel (\d -> d |> DiceModel.setHistorySize length) card

clearHistory : MixedCard -> MixedCard
clearHistory card =
    mapDiceModel (\d -> d |> DiceModel.clearHistory) card

encode : MixedCard -> Encode.Value
encode card =
    Encode.object
        [ ("name", Encode.string card.name)
        , ("dieFaces", Encode.list Encode.int card.dieFaces)
        , ("id", Encode.string (card.id |> UUID.toString))
        , ("explodes", Encode.bool card.dice.explodes)
        , ("maxHistory", Encode.int card.dice.maxHistory)
        ]

encodeMultiple : List MixedCard -> Encode.Value
encodeMultiple cards =
    Encode.list encode cards

decoder : Decode.Decoder JsonModel
decoder =
    Decode.map5 JsonModel
        (Decode.field "name" Decode.string)
        (Decode.field "dieFaces" (Decode.list Decode.int))
        (Decode.field "id" Decode.string)
        (Decode.field "explodes" Decode.bool)
        (Decode.field "maxHistory" Decode.int)

fromJsonModel : JsonModel -> Maybe MixedCard
fromJsonModel json =
    case UUID.fromString json.id of
       Ok parsedId -> Just (createWithoutHistory json.name json.dieFaces json.explodes json.maxHistory parsedId)
       Err _ -> Nothing

decodeMultipleFromString : String -> Result Decode.Error (List MixedCard)
decodeMultipleFromString json =
    case Decode.decodeString Decode.value json of
       Ok j -> decodeMultiple j
       Err e -> Err e

decodeMultiple : Decode.Value -> Result Decode.Error (List MixedCard)
decodeMultiple json =
    let d = Decode.map (List.map fromJsonModel) (Decode.list decoder)
    in
        case Decode.decodeValue d json of
            Ok cards -> Ok (cards |> Maybe.Extra.values)
            Err e -> Err e