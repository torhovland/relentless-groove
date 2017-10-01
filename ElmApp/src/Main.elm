port module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Navigation


-- Main


main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Ports


port authenticated : (AuthenticatedData -> msg) -> Sub msg



-- Model


type alias AuthenticatedData =
    { name : String
    , image_url : String
    , id_token : String
    }


type alias Model =
    { authenticatedData : AuthenticatedData
    , number : Int
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( { authenticatedData = AuthenticatedData "" "" ""
      , number = 0
      }
    , Cmd.none
    )



-- Messages


type Msg
    = UrlChange Navigation.Location
    | Authenticated AuthenticatedData
    | Increment
    | Decrement



-- Update


update msg model =
    case msg of
        UrlChange url ->
            -- Not supporting change of url at the moment, just return the model unchanged, with no side effect.
            ( model, Cmd.none )

        Increment ->
            ( { model | number = model.number + 1 }, Cmd.none )

        Decrement ->
            ( { model | number = model.number - 1 }, Cmd.none )

        Authenticated msg ->
            ( { model | authenticatedData = msg }, Cmd.none )



-- View


view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (toString model.number) ]
        , button [ onClick Increment ] [ text "+" ]
        , div [] [ text (toString model.authenticatedData.name) ]
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ authenticated Authenticated
        ]
