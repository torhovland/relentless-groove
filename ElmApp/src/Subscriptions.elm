module Subscriptions exposing (subscriptions)

import Model exposing (Model, Msg)
import Ports


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.authenticated Model.Authenticated
        ]
