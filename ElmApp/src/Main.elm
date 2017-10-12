port module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (list, string)
import Navigation
import Numeral exposing (format)
import Time.DateTime exposing (DateTime, fromTimestamp, fromTuple, minute, second, toISO8601, toTimestamp)


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


type alias LogEntry =
    { start : DateTime
    , end : DateTime
    }


type alias Activity =
    { name : String
    , minutesPerWeek : Int
    , log : List LogEntry
    }


type alias Model =
    { apiUrl : String
    , authenticatedData : AuthenticatedData
    , activities : List Activity
    , number : Int
    }


initActivity1 =
    let
        start =
            fromTuple ( 2010, 10, 10, 10, 0, 0, 0 )

        end =
            fromTuple ( 2010, 10, 10, 10, 10, 0, 0 )
    in
    Activity "foo" 15 [ LogEntry start end ]


initActivity2 =
    let
        start =
            fromTuple ( 2010, 10, 10, 10, 0, 0, 0 )

        end =
            fromTuple ( 2010, 10, 10, 10, 10, 0, 0 )
    in
    Activity "bar" 30 [ LogEntry start end ]


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    ( { apiUrl = flags.apiUrl
      , authenticatedData = AuthenticatedData "" "" ""
      , activities = [ initActivity1, initActivity2 ]
      , number = 0
      }
    , Cmd.none
    )


activityGoal : Activity -> Float
activityGoal activity =
    toFloat activity.minutesPerWeek * 60.0 * 1000.0


duration : LogEntry -> Float
duration logEntry =
    toTimestamp logEntry.end - toTimestamp logEntry.start


loggedTime : Activity -> Float
loggedTime activity =
    activity.log
        |> List.map duration
        |> List.sum


onScheduleRatio : Activity -> Float
onScheduleRatio activity =
    loggedTime activity / activityGoal activity


remaining : Activity -> DateTime
remaining activity =
    fromTimestamp (activityGoal activity - loggedTime activity)



-- Messages


type Msg
    = UrlChange Navigation.Location
    | Authenticated AuthenticatedData
    | Increment
    | Decrement
    | PostActivity
    | PostActivityResult (Result Http.Error ())



-- Update


postActivity apiUrl =
    let
        url =
            apiUrl ++ "/activities"

        body =
            Http.stringBody "application/json" "{ name: 'hallo', minutesPerWeek: 15 }"
    in
    Http.request
        { method = "POST"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }
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
            ( { model | authenticatedData = Debug.log "authenticated: " msg }, Cmd.none )



-- View


formatTime time =
    let
        totalSeconds time =
            toTimestamp time / 1000.0
    in
    format "00:00:00" (totalSeconds time)


activityView : Activity -> Html Msg
activityView activity =
    div []
        [ div [] [ text activity.name ]
        , div [] [ text (format "0 %" (onScheduleRatio activity) ++ " on schedule.") ]
        , div [] [ text (formatTime (remaining activity) ++ " remaining.") ]
        ]


topActivitiesView model =
    div [] (List.map activityView model.activities)


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (toString model.number) ]
        , button [ onClick Increment ] [ text "+" ]
        , div [] [ text (toString model.authenticatedData.name) ]
        , button [ onClick PostActivity ] [ text "Post activity" ]
        , topActivitiesView model
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ authenticated Authenticated
        ]
