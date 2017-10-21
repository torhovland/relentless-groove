module Subscriptions exposing (subscriptions)

import Material
import Model exposing (Model, Msg)
import Ports
import Time


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second Model.Tick
        , Ports.authenticated Model.Authenticated
        , Material.subscriptions Model.Mdl model
        ]
