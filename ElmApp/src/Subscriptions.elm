module Subscriptions exposing (subscriptions)

import Model exposing (..)
import Ports exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.authenticated Authenticated
        ]
