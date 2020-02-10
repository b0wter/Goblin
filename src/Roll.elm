module Roll exposing (Single, Multi, Combi, Roll, singleRandomGenerator, multiRandomGenerator)

import Random

type alias Single =
    { die : Int
    , result: Int
    }

type alias Multi =
    { die: Int
    , result: List Int
    }

type Roll
    = SingleRoll Single
    | MultiRoll Multi

type alias Combi = List Single

singleRandomGenerator: Int -> Random.Generator Int
singleRandomGenerator faceCount = Random.int 1 faceCount

multiRandomGenerator : Int -> Int -> Random.Generator (List Int)
multiRandomGenerator faceCount diceCount = Random.list diceCount (Random.int 1 faceCount)