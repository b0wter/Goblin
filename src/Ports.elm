port module Ports exposing (StorageObject, store, retrieve, requestRetrieval)

import Json.Encode as E

type alias StorageObject = 
    {
        key: String,
        value: String
    }

--port store : E.Value -> Cmd msg
port store : StorageObject -> Cmd msg

port retrieve : (StorageObject -> msg) -> Sub msg

port requestRetrieval : String -> Cmd msg