module Main exposing (..)

import Model exposing (..)
import Navigation
import Subscriptions
import Update
import View


main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = Subscriptions.subscriptions
        }
