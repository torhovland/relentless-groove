module Model
    exposing
        ( Activity
        , AuthenticatedData
        , Model
        , Msg(Authenticated, Decrement, Increment, NewUrl, PostActivity, PostActivityResult, UrlChange)
        , Route(Activities, Home, Log, Tomorrow)
        , init
        , onScheduleRatioString
        , remainingString
        , route
        , topActivities
        )

import Http
import Navigation
import Numeral exposing (format)
import Time.DateTime exposing (DateTime, fromTimestamp, fromTuple, toTimestamp)
import UrlParser as Url


type Route
    = Home
    | Tomorrow
    | Activities
    | Log


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Home Url.top
        , Url.map Tomorrow (Url.s "tomorrow")
        , Url.map Activities (Url.s "activities")
        , Url.map Log (Url.s "log")
        ]


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
    { location : Maybe Route
    , apiUrl : String
    , authenticatedData : AuthenticatedData
    , errorMessage : String
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


remaining : Activity -> DateTime
remaining activity =
    fromTimestamp (activityGoal activity - loggedTime activity)


onScheduleRatio : Activity -> Float
onScheduleRatio activity =
    loggedTime activity / activityGoal activity


onScheduleSort : Activity -> Activity -> Order
onScheduleSort a b =
    compare (onScheduleRatio a) (onScheduleRatio b)


topActivities : List Activity -> List Activity
topActivities =
    List.sortWith onScheduleSort >> List.take 3


formatTime : DateTime -> String
formatTime =
    toTimestamp >> (/) 1000.0 >> format "00:00:00"


formatPercent : Float -> String
formatPercent =
    format "0 %"


remainingString : Activity -> String
remainingString =
    remaining >> formatTime


onScheduleRatioString : Activity -> String
onScheduleRatioString =
    onScheduleRatio >> formatPercent


initActivity1 : Activity
initActivity1 =
    let
        start =
            fromTuple ( 2010, 10, 10, 10, 0, 0, 0 )

        end =
            fromTuple ( 2010, 10, 10, 10, 10, 0, 0 )
    in
    Activity "foo" 15 [ LogEntry start end ]


initActivity2 : Activity
initActivity2 =
    let
        start =
            fromTuple ( 2010, 10, 10, 10, 0, 0, 0 )

        end =
            fromTuple ( 2010, 10, 10, 10, 10, 0, 0 )
    in
    Activity "bar" 30 [ LogEntry start end ]


initActivity3 : Activity
initActivity3 =
    let
        start =
            fromTuple ( 2010, 10, 10, 10, 0, 0, 0 )

        end =
            fromTuple ( 2010, 10, 10, 10, 10, 0, 0 )
    in
    Activity "hello" 120 [ LogEntry start end ]


initActivity4 : Activity
initActivity4 =
    let
        start =
            fromTuple ( 2010, 10, 10, 10, 0, 0, 0 )

        end =
            fromTuple ( 2010, 10, 10, 10, 10, 0, 0 )
    in
    Activity "world" 60 [ LogEntry start end ]


init : String -> Navigation.Location -> ( Model, Cmd Msg )
init apiUrl location =
    ( { location = Url.parsePath route location
      , apiUrl = apiUrl
      , authenticatedData = AuthenticatedData "" "" ""
      , errorMessage = ""
      , activities = [ initActivity1, initActivity2, initActivity3, initActivity4 ]
      , number = 0
      }
    , Cmd.none
    )


type Msg
    = NewUrl String
    | UrlChange Navigation.Location
    | Authenticated AuthenticatedData
    | Increment
    | Decrement
    | PostActivity
    | PostActivityResult (Result Http.Error ())