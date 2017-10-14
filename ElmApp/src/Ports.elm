port module Ports exposing (..)

import Model exposing (..)


port authenticated : (AuthenticatedData -> msg) -> Sub msg
