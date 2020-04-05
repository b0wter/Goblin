port module Ports exposing (StorageObject, store, retrieve, requestRetrieval, createStorageObject)

import Json.Encode as Encode

type alias StorageObject = 
    {
        key: String,
        value: String
    }

createStorageObject : String -> Encode.Value -> StorageObject
createStorageObject key value =
    {
        key = key,
        value = value |> Encode.encode 0
    }

port store : StorageObject -> Cmd msg

port retrieve : (StorageObject -> msg) -> Sub msg

port requestRetrieval : String -> Cmd msg