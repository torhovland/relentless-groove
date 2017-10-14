module Model exposing (..)

import Http
import Json.Decode exposing (list, string)
import Navigation
import Numeral exposing (format)
import Time.DateTime exposing (DateTime, fromTimestamp, fromTuple, toTimestamp)


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


formatTime time =
    let
        totalSeconds time =
            toTimestamp time / 1000.0
    in
    format "00:00:00" (totalSeconds time)


formatPercent num =
    format "0 %" num


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


initActivity3 =
    let
        start =
            fromTuple ( 2010, 10, 10, 10, 0, 0, 0 )

        end =
            fromTuple ( 2010, 10, 10, 10, 10, 0, 0 )
    in
    Activity "hello" 120 [ LogEntry start end ]


initActivity4 =
    let
        start =
            fromTuple ( 2010, 10, 10, 10, 0, 0, 0 )

        end =
            fromTuple ( 2010, 10, 10, 10, 10, 0, 0 )
    in
    Activity "world" 60 [ LogEntry start end ]


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    ( { apiUrl = flags.apiUrl
      , authenticatedData = AuthenticatedData "" "" ""
      , activities = [ initActivity1, initActivity2, initActivity3, initActivity4 ]
      , number = 0
      }
    , Cmd.none
    )


type Msg
    = UrlChange Navigation.Location
    | Authenticated AuthenticatedData
    | Increment
    | Decrement
    | PostActivity
    | PostActivityResult (Result Http.Error ())
