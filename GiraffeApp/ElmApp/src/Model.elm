module Model
    exposing
        ( Activity
        , ActivityEdit
        , AuthenticatedData
        , LogEntry
        , Model
        , Msg
            ( Authenticated
            , ChangeActivityImage
            , ChangeActivityName
            , ChangeActivitySlider
            , Mdl
            , NewActivityId
            , NewUrl
            , PostActivityResult
            , SaveActivityType
            , StartActivity
            , StopActivity
            , Tick
            , UrlChange
            )
        , Route(Activities, Home, Log, LogActivity, NewActivity, Tomorrow)
        , activityById
        , formatMinutes
        , init
        , initActivity
        , initActivityEdit
        , isActivityRunning
        , minutesPerWeek
        , onScheduleRatioString
        , remainingString
        , route
        , sortedActivities
        , topActivities
        , updatedActivity
        )

import Http
import Material
import Navigation
import Numeral exposing (format)
import Time exposing (Time)
import UrlParser as Url exposing ((</>))


type Route
    = Home
    | Tomorrow
    | Log
    | Activities
    | NewActivity
    | LogActivity Int


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Home Url.top
        , Url.map Tomorrow (Url.s "tomorrow")
        , Url.map Log (Url.s "log")
        , Url.map Activities (Url.s "activities")
        , Url.map NewActivity (Url.s "new-activity")
        , Url.map LogActivity (Url.s "log-activity" </> Url.int)
        ]


type alias AuthenticatedData =
    { name : String
    , image_url : String
    , id_token : String
    }


type alias LogEntry =
    { start : Time
    , end : Maybe Time
    }


type alias Activity =
    { id : Int
    , name : String
    , imageUrl : String
    , minutesPerWeek : Int
    , log : List LogEntry
    }


type alias ActivityEdit =
    { activity : Activity
    , sliderValue : Int
    }


type alias Model =
    { time : Time
    , location : Maybe Route
    , apiUrl : String
    , authenticatedData : AuthenticatedData
    , errorMessage : String
    , mdl : Material.Model
    , activities : List Activity
    , activityEdit : ActivityEdit
    }


activityById : Int -> List Activity -> Maybe Activity
activityById id activities =
    activities
        |> List.filter (\a -> a.id == id)
        |> List.head


activityGoal : Activity -> Float
activityGoal activity =
    toFloat activity.minutesPerWeek * 60.0 * 1000.0


duration : Model -> LogEntry -> Float
duration model logEntry =
    Maybe.withDefault model.time logEntry.end - logEntry.start


loggedTime : Model -> Activity -> Float
loggedTime model activity =
    activity.log
        |> List.map (duration model)
        |> List.sum


remaining : Model -> Activity -> Time
remaining model activity =
    activityGoal activity - loggedTime model activity


onScheduleRatio : Model -> Activity -> Float
onScheduleRatio model activity =
    loggedTime model activity / activityGoal activity


topActivities : Model -> List Activity
topActivities model =
    let
        onScheduleSort a b =
            compare (onScheduleRatio model a) (onScheduleRatio model b)
    in
    model.activities
        |> List.sortWith onScheduleSort
        |> List.take 3


sortedActivities : List Activity -> List Activity
sortedActivities =
    let
        lexicalSort a b =
            compare a.name b.name
    in
    List.sortWith lexicalSort


isActivityRunning : Activity -> Bool
isActivityRunning activity =
    activity.log
        |> List.filter (\l -> l.end == Nothing)
        |> List.isEmpty
        |> not


toMinutes : Time -> Int
toMinutes time =
    round (time / 1000 / 60)


formatTimeShort : Time -> String
formatTimeShort time =
    format "00:00:00" (time / 1000)


formatTimeLong : Time -> String
formatTimeLong =
    toMinutes >> formatMinutes


formatPercent : Float -> String
formatPercent =
    format "0 %"


remainingString : Model -> Activity -> String
remainingString model =
    remaining model >> formatTimeLong


onScheduleRatioString : Model -> Activity -> String
onScheduleRatioString model =
    onScheduleRatio model >> formatPercent


formatMinutes : Int -> String
formatMinutes minutes =
    let
        hoursPart =
            if minutes >= 120 then
                toString (minutes // 60) ++ " hours"
            else if minutes >= 60 then
                "1 hour"
            else
                ""

        remainingMinutes =
            minutes % 60

        minutesPart =
            if remainingMinutes > 0 then
                toString remainingMinutes ++ " minutes"
            else
                ""
    in
    if (hoursPart /= "") && (minutesPart /= "") then
        hoursPart ++ " and " ++ minutesPart
    else if minutesPart /= "" then
        minutesPart
    else if hoursPart /= "" then
        hoursPart
    else
        ""


minutesPerWeek : ActivityEdit -> Int
minutesPerWeek activity =
    let
        slider =
            activity.sliderValue
    in
    if slider <= 12 then
        slider * 5
    else if slider <= 20 then
        12 * 5 + (slider - 12) * 15
    else
        12 * 5 + 8 * 15 + (slider - 20) * 30


updatedActivity : ActivityEdit -> Activity
updatedActivity activityEdit =
    let
        activity =
            activityEdit.activity
    in
    { activity | minutesPerWeek = minutesPerWeek activityEdit }


initActivity : Activity
initActivity =
    Activity -1 "" "" 15 []


initActivityEdit : ActivityEdit
initActivityEdit =
    ActivityEdit initActivity 3


initActivity1 : Activity
initActivity1 =
    Activity 1
        "foo"
        "http://www.contentwritingshop.co.uk/wp-content/uploads/content-writing-1200x800.jpg"
        15
        [ LogEntry 0 (Just <| 10 * 60 * 1000) ]


initActivity2 : Activity
initActivity2 =
    Activity 2
        "bar"
        "https://www.passion4dancing.com/wp-content/uploads/2015/10/Dance-confidence.jpg"
        30
        [ LogEntry 0 (Just <| 10 * 60 * 1000) ]


initActivity3 : Activity
initActivity3 =
    Activity 3
        "hello"
        "https://theredlist.com/media/database/muses/icon/sport/cycling/030-cycling-theredlist.jpg"
        120
        [ LogEntry 0 (Just <| 10 * 60 * 1000) ]


initActivity4 : Activity
initActivity4 =
    Activity 4
        "world"
        "https://i0.wp.com/www.flandersfamily.info/web/wp-content/uploads/2011/07/Chores.png"
        60
        [ LogEntry 0 (Just <| 10 * 60 * 1000) ]


init : String -> Navigation.Location -> ( Model, Cmd Msg )
init apiUrl location =
    ( { time = 0
      , location = Url.parsePath route location
      , apiUrl = apiUrl
      , authenticatedData = AuthenticatedData "" "" ""
      , errorMessage = ""
      , mdl = Material.model
      , activities = [ initActivity1, initActivity2, initActivity3, initActivity4 ]
      , activityEdit = initActivityEdit
      }
    , Material.init Mdl
    )


type Msg
    = Tick Time
    | NewUrl String
    | UrlChange Navigation.Location
    | Authenticated AuthenticatedData
    | Mdl (Material.Msg Msg)
    | NewActivityId Int
    | ChangeActivityName String
    | ChangeActivityImage String
    | ChangeActivitySlider Float
    | SaveActivityType
    | PostActivityResult (Result Http.Error ())
    | StartActivity Int
    | StopActivity Int
