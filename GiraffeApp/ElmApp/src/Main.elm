module Main exposing (main)

import Model exposing (Model, Msg)
import Navigation
import Subscriptions
import Update
import View


main : Program String Model Msg
main =
    Navigation.programWithFlags Model.UrlChange
        { init = Model.init
        , view = View.view
        , update = Update.update
        , subscriptions = Subscriptions.subscriptions
        }
