module Roll exposing (Single, Multi, Mixed, Roll, singleRandomGenerator, multiRandomGenerator, die, asSingleRoll, asMultiRoll, asMixedRoll)

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
    | MixedRoll Mixed

asSingleRoll: Single -> Roll
asSingleRoll r = SingleRoll r

asMultiRoll: Multi -> Roll
asMultiRoll r = MultiRoll r

asMixedRoll: Mixed -> Roll
asMixedRoll r = MixedRoll r

singleRandomGenerator: Int -> Random.Generator Int
singleRandomGenerator faceCount = Random.int 1 faceCount

multiRandomGenerator : Int -> Int -> Random.Generator (List Int)
multiRandomGenerator faceCount diceCount = Random.list diceCount (Random.int 1 faceCount)

die: Roll -> Int
die roll =
    let stringToIntOrZero s = 
         case s |> String.toInt of
             Just i -> i
             Nothing -> 0
    in
        case roll of
            SingleRoll r -> r.die
            MultiRoll r -> r.die
            MixedRoll rolls -> rolls |> List.map (\r -> r.die |> String.fromInt) |> String.join "" |> stringToIntOrZero
