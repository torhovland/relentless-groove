port module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (list, string)
import Navigation


-- Main


main =
    Navigation.programWithFlags UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Ports


port authenticated : (AuthenticatedData -> msg) -> Sub msg



-- Model


type alias Flags =
    { apiUrl : String
    }


type alias AuthenticatedData =
    { name : String
    , image_url : String
    , id_token : String
    }


type alias Model =
    { apiUrl : String
    , authenticatedData : AuthenticatedData
    , number : Int
    }


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    ( { apiUrl = flags.apiUrl
      , authenticatedData = AuthenticatedData "" "" ""
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
    | PostActivity
    | PostActivityResult (Result Http.Error (List String))



-- Update


postActivity apiUrl =
    let
        url =
            apiUrl ++ "/activities"
    in
    Http.post url (Http.stringBody "application/json" "{ name: 'hallo', minutesPerWeek: 15 }") (list string)
        |> Http.send PostActivityResult


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange url ->
            -- Not supporting change of url at the moment, just return the model unchanged, with no side effect.
            ( model, Cmd.none )

        Increment ->
            ( { model | number = model.number + 1 }, Cmd.none )

        Decrement ->
            ( { model | number = model.number - 1 }, Cmd.none )

        PostActivity ->
            ( model, postActivity model.apiUrl )

        PostActivityResult (Ok statistics) ->
            ( model, Cmd.none )

        PostActivityResult (Err failure) ->
            let
                log =
                    Debug.log "error" failure
            in
            ( model, Cmd.none )

        Authenticated msg ->
            ( { model | authenticatedData = msg }, Cmd.none )



-- View


view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (toString model.number) ]
        , button [ onClick Increment ] [ text "+" ]
        , div [] [ text (toString model.authenticatedData.name) ]
        , button [ onClick PostActivity ] [ text "Post activity" ]
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ authenticated Authenticated
        ]
