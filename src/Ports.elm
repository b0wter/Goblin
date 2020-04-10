port module Ports exposing (StorageObject, store, retrieve, createStorageObject, createSerializedStorageObject)

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

createSerializedStorageObject : String -> String -> StorageObject
createSerializedStorageObject key value =
    { 
        key = key,
        value = value
    }

port store : StorageObject -> Cmd msg

port retrieve : (StorageObject -> msg) -> Sub msg
