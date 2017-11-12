module View
    exposing
        ( ViewMessages
        , activitiesPageView
        , drawer
        , editActivityPageView
        , homePageView
        , logActivityPageView
        , logPageView
        , pageHeader
        , signinView
        , tomorrowPageView
        , unknownActivityPageView
        , unknownPageView
        )

import Activity exposing (Activity, ActivityEdit)
import Html exposing (Html, div, header, img, p, text)
import Html.Attributes exposing (class, id, src)
import Material
import Material.Button
import Material.Card as Card
import Material.Elevation
import Material.Grid as Grid
import Material.Icon
import Material.Layout
import Material.Options
import Material.Progress
import Material.Slider
import Material.Textfield
import Material.Typography
import Numeral
import Time exposing (Time)


type alias ViewMessages message =
    { materialMsgHandler : Material.Msg message -> message
    , changeActivityNameHandler : String -> message
    , changeActivityImageHandler : String -> message
    , changeActivitySliderHandler : Float -> message
    , newUrlHandler : String -> message
    , saveActivityTypeHandler : message
    , startActivityHandler : Int -> message
    , stopActivityHandler : Int -> message
    }


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


toMinutes : Time -> Int
toMinutes time =
    round (time / 1000 / 60)


formatTimeLong : Time -> String
formatTimeLong =
    toMinutes >> formatMinutes


formatPercent : Float -> String
formatPercent =
    Numeral.format "0 %"


remainingString : Time -> Activity -> String
remainingString time =
    Activity.remaining time >> formatTimeLong


onScheduleRatioString : Time -> Activity -> String
onScheduleRatioString time =
    Activity.onScheduleRatio time >> formatPercent


activityView : Time -> Activity -> Html msg
activityView time activity =
    Card.view
        [ Material.Options.css "margin" "1em"
        , Material.Options.css "width" "100%"
        , Material.Elevation.e2
        ]
        [ Card.title
            [ Material.Options.css "align-content" "flex-start"
            , Material.Options.css "flex-direction" "row"
            , Material.Options.css "align-items" "flex-start"
            , Material.Options.css "justify-content" "space-between"
            ]
            [ Material.Options.div []
                [ Card.head [] [ text activity.name ]
                , Card.subhead [] [ text <| remainingString time activity ++ " to do" ]
                ]
            , Material.Options.img
                [ Material.Options.attribute <| Html.Attributes.src activity.imageUrl
                , Material.Options.css "width" "5em"
                ]
                []
            ]
        , Card.text []
            [ Material.Options.styled p
                [ Material.Typography.headline ]
                [ text <| onScheduleRatioString time activity ++ " on schedule" ]
            , Material.Progress.progress <| 100 * Activity.onScheduleRatio time activity
            ]
        ]


activitiesView : Time -> List Activity -> Html msg
activitiesView time =
    List.map (activityView time) >> Material.Options.div []


homePageView : Time -> List Activity -> Html msg
homePageView time activities =
    div []
        [ Material.Options.styled Html.h1
            [ Material.Typography.headline ]
            [ text "To do today" ]
        , Material.Options.styled Html.h2
            [ Material.Typography.subhead ]
            [ text "Choose one of these now" ]
        , Grid.grid []
            [ Grid.cell
                [ Grid.size Grid.Tablet 8, Grid.size Grid.Desktop 6, Grid.offset Grid.Desktop 3 ]
                [ activities |> activitiesView time ]
            ]
        ]


tomorrowPageView : Html msg
tomorrowPageView =
    div []
        [ Material.Options.styled Html.h1
            [ Material.Typography.headline ]
            [ text "To do tomorrow" ]
        , Material.Options.styled Html.h2
            [ Material.Typography.subhead ]
            [ text "Unless you do some of it today" ]
        ]


activitiesPageView : ViewMessages msg -> Material.Model -> Time -> List Activity -> Html msg
activitiesPageView messages mdl time activities =
    div [ class "fullpage" ]
        [ Material.Options.styled Html.h1
            [ Material.Typography.headline ]
            [ text "All your relentless activities" ]
        , Grid.grid []
            [ Grid.cell
                [ Grid.size Grid.Tablet 8, Grid.size Grid.Desktop 6, Grid.offset Grid.Desktop 3 ]
                [ activities |> Activity.sorted |> activitiesView time ]
            ]
        , div [ class "bottomright" ]
            [ Material.Button.render messages.materialMsgHandler
                [ 0 ]
                mdl
                [ Material.Button.fab
                , Material.Button.colored
                , Material.Button.ripple
                , Material.Options.onClick <| messages.newUrlHandler "/new-activity"
                ]
                [ Material.Icon.i "add" ]
            ]
        ]


logPageView : Html msg
logPageView =
    div []
        [ Material.Options.styled Html.h1
            [ Material.Typography.headline ]
            [ text "Activity log" ]
        ]


editActivityPageView : ViewMessages message -> Material.Model -> ActivityEdit -> Html message
editActivityPageView messages mdl activityEdit =
    div []
        [ div []
            [ Material.Options.styled Html.h1
                [ Material.Typography.headline ]
                [ text "New activity type" ]
            , Material.Options.styled Html.h2
                [ Material.Typography.title ]
                [ text "Activity details" ]
            , Material.Textfield.render messages.materialMsgHandler
                [ 0 ]
                mdl
                [ Material.Textfield.label "Name of activity type"
                , Material.Textfield.floatingLabel
                , Material.Textfield.value activityEdit.activity.name
                , Material.Options.onInput messages.changeActivityNameHandler
                ]
                []
            ]
        , div []
            [ Material.Textfield.render messages.materialMsgHandler
                [ 0 ]
                mdl
                [ Material.Textfield.label "URL to activity icon"
                , Material.Textfield.floatingLabel
                , Material.Textfield.value activityEdit.activity.imageUrl
                , Material.Options.onInput messages.changeActivityImageHandler
                ]
                []
            ]
        , div []
            [ Material.Options.styled Html.h2
                [ Material.Typography.title ]
                [ text "Relentlessness of the activity" ]
            , Material.Slider.view
                [ Material.Slider.onChange messages.changeActivitySliderHandler
                , Material.Slider.value <| toFloat activityEdit.sliderValue
                , Material.Slider.max 34
                , Material.Slider.min 1
                , Material.Slider.step 1
                ]
            , Material.Options.styled p
                [ Material.Typography.body1 ]
                [ text <| formatMinutes (Activity.minutesPerWeek activityEdit) ++ " per week." ]
            ]
        , div []
            [ Material.Button.render messages.materialMsgHandler
                [ 0 ]
                mdl
                [ Material.Button.raised
                , Material.Button.colored
                , Material.Button.ripple
                , Material.Options.onClick messages.saveActivityTypeHandler
                ]
                [ text "Save activity type" ]
            ]
        ]


logActivityPageView : ViewMessages message -> Material.Model -> Activity -> Html message
logActivityPageView messages mdl activity =
    let
        isRunning =
            Activity.isRunning activity
    in
    div []
        [ Material.Options.styled p
            [ Material.Typography.headline ]
            [ text "Log time on activity" ]
        , Material.Options.styled p
            [ Material.Typography.subhead ]
            [ text <| activity.name ]
        , Material.Button.render messages.materialMsgHandler
            [ 0 ]
            mdl
            [ Material.Button.raised
            , Material.Button.colored
            , Material.Button.ripple
            , Material.Options.onClick <|
                if isRunning then
                    messages.stopActivityHandler activity.id
                else
                    messages.startActivityHandler activity.id
            ]
            [ text <|
                if isRunning then
                    "Stop activity"
                else
                    "Start activity"
            ]
        ]


unknownActivityPageView : Html msg
unknownActivityPageView =
    div []
        [ Material.Options.styled p
            [ Material.Typography.headline ]
            [ text "Unknown activity id" ]
        ]


unknownPageView : Html msg
unknownPageView =
    div []
        [ Material.Options.styled p
            [ Material.Typography.headline ]
            [ text "Unknown page" ]
        ]


pageHeader : Html msg
pageHeader =
    Material.Layout.row []
        [ img [ class "logo", src "images/weather.svg" ] []
        , Material.Layout.title [] [ text "Relentless Groove" ]
        ]


drawer : ViewMessages message -> String -> String -> List (Html message)
drawer messages userName userImageUrl =
    [ header [ class "drawer-header" ]
        [ img [ class "avatar", src userImageUrl ] []
        , div [ class "name" ] [ text userName ]
        ]
    , Material.Layout.navigation
        []
        [ Material.Layout.link
            [ Material.Options.onClick <| messages.newUrlHandler "/" ]
            [ text "Today" ]
        , Material.Layout.link
            [ Material.Options.onClick <| messages.newUrlHandler "/tomorrow" ]
            [ text "Tomorrow" ]
        , Material.Layout.link
            [ Material.Options.onClick <| messages.newUrlHandler "/log" ]
            [ text "Log" ]
        , Material.Layout.link
            [ Material.Options.onClick <| messages.newUrlHandler "/activities" ]
            [ text "Activities" ]
        , Material.Layout.link
            [ Material.Options.onClick <| messages.newUrlHandler "/new-activity" ]
            [ text "New activity type" ]
        ]
    ]


signinView : Bool -> Html msg
signinView isVisible =
    div
        [ id "my-signin2"
        , class <|
            if isVisible then
                "visible"
            else
                "invisible"
        ]
        []
