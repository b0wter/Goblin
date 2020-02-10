module Roll exposing (Single, Multi, Combi, Roll)

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
