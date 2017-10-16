module Subscriptions exposing (subscriptions)

import Material
import Model exposing (Model, Msg)
import Ports


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.authenticated Model.Authenticated
        , Material.subscriptions Model.Mdl model
        ]
