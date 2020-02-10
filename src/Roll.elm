module Roll exposing (Single, Multi, Mixed, Roll, singleRandomGenerator, multiRandomGenerator)

import Random

type alias Single =
    { die : Int
    , result: Int
    }

type alias Multi =
    { die: Int
    , result: List Int
    }

type alias Mixed = List Single

type Roll
    = SingleRoll Single
    | MultiRoll Multi

singleRandomGenerator: Int -> Random.Generator Int
singleRandomGenerator faceCount = Random.int 1 faceCount

multiRandomGenerator : Int -> Int -> Random.Generator (List Int)
multiRandomGenerator faceCount diceCount = Random.list diceCount (Random.int 1 faceCount)