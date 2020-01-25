module Roll exposing (Single, Multi, Combi)

type alias Single =
    { die : Int
    , result: Int
    }

type alias Multi =
    { die: Int
    , result: List Int
    }

type alias Combi = List Single
