port module Ports exposing (authenticated)

import Model


port authenticated : (Model.AuthenticatedData -> msg) -> Sub msg
