module DebugOutput exposing ( Message
                            , messageAsAlert 
                            , createInfo
                            , createWarning
                            , createError
                            , createSuccess
                            )

import Bootstrap.Alert as Alert
import Html exposing (..)
import Html.Attributes exposing (..)

type Message
    = Info String
    | Warning String
    | Error String
    | Success String

messageAsAlert : Message -> Html msg
messageAsAlert message =
    case message of
        Info t -> Alert.simpleInfo [] [ text t ]
        Warning t -> Alert.simpleWarning [] [ text t ]
        Error t -> Alert.simpleDanger [] [ text t ]
        Success t -> Alert.simpleSuccess [] [ text t ]

createInfo : String -> Message
createInfo text = Info text

createWarning : String -> Message
createWarning text = Warning text

createError : String -> Message
createError text = Error text

createSuccess : String -> Message
createSuccess text = Success text
