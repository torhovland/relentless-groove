module Activity
    exposing
        ( Activity
        , byId
        , initActivities
        , isRunning
        , onScheduleRatio
        , remaining
        , sorted
        , start
        , stop
        , top
        )

import Time exposing (Time)


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


byId : Int -> List Activity -> Maybe Activity
byId id =
    List.filter (\a -> a.id == id) >> List.head


remaining : Time -> Activity -> Time
remaining time activity =
    activityGoal activity - loggedTime time activity


top : Time -> List Activity -> List Activity
top time activities =
    let
        onScheduleSort a b =
            compare (onScheduleRatio time a) (onScheduleRatio time b)
    in
    activities
        |> List.sortWith onScheduleSort
        |> List.take 3


sorted : List Activity -> List Activity
sorted =
    let
        lexicalSort a b =
            compare a.name b.name
    in
    List.sortWith lexicalSort


isRunning : Activity -> Bool
isRunning =
    .log
        >> List.filter (\l -> l.end == Nothing)
        >> List.isEmpty
        >> not


start : Int -> Time -> List Activity -> List Activity
start id time activities =
    List.map
        (\a ->
            if a.id == id then
                { a | log = LogEntry time Nothing :: a.log }
            else
                a
        )
        activities


stop : Int -> Time -> List Activity -> List Activity
stop id time activities =
    let
        updateEnd logEntry =
            if logEntry.end == Nothing then
                { logEntry | end = Just time }
            else
                logEntry
    in
    List.map
        (\a ->
            if a.id == id then
                { a | log = List.map updateEnd a.log }
            else
                a
        )
        activities


duration : Time -> LogEntry -> Time
duration time logEntry =
    Maybe.withDefault time logEntry.end - logEntry.start


loggedTime : Time -> Activity -> Time
loggedTime time activity =
    activity.log
        |> List.map (duration time)
        |> List.sum


activityGoal : Activity -> Float
activityGoal activity =
    toFloat activity.minutesPerWeek * 60.0 * 1000.0


onScheduleRatio : Time -> Activity -> Float
onScheduleRatio time activity =
    loggedTime time activity / activityGoal activity


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


initActivities : List Activity
initActivities =
    [ initActivity1, initActivity2, initActivity3, initActivity4 ]
