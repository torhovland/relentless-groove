port module Main exposing (main)

import Activity exposing (Activity)
import Html exposing (Html, div, header, img, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, id, src)
import Http
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
import Navigation
import Numeral exposing (format)
import Random
import Time exposing (Time)
import UrlParser as Url exposing ((</>))


port authenticated : (AuthenticatedData -> msg) -> Sub msg


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


init : String -> Navigation.Location -> ( Model, Cmd Msg )
init apiUrl location =
    ( { time = 0
      , location = Url.parsePath route location
      , apiUrl = apiUrl
      , authenticatedData = AuthenticatedData "" "" ""
      , errorMessage = ""
      , mdl = Material.model
      , activities = Activity.initActivities
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


formatTimeShort : Time -> String
formatTimeShort time =
    format "00:00:00" (time / 1000)


formatTimeLong : Time -> String
formatTimeLong =
    toMinutes >> formatMinutes


formatPercent : Float -> String
formatPercent =
    format "0 %"


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


activitiesView : Model -> List Activity -> Html Msg
activitiesView model =
    List.map (activityView model.time) >> Material.Options.div []


editActivityView : Material.Model -> ActivityEdit -> Html Msg
editActivityView mdl activityEdit =
    div []
        [ div []
            [ Material.Options.styled Html.h1
                [ Material.Typography.headline ]
                [ text "New activity type" ]
            , Material.Options.styled Html.h2
                [ Material.Typography.title ]
                [ text "Activity details" ]
            , Material.Textfield.render Mdl
                [ 0 ]
                mdl
                [ Material.Textfield.label "Name of activity type"
                , Material.Textfield.floatingLabel
                , Material.Textfield.value activityEdit.activity.name
                , Material.Options.onInput ChangeActivityName
                ]
                []
            ]
        , div []
            [ Material.Textfield.render Mdl
                [ 0 ]
                mdl
                [ Material.Textfield.label "URL to activity icon"
                , Material.Textfield.floatingLabel
                , Material.Textfield.value activityEdit.activity.imageUrl
                , Material.Options.onInput ChangeActivityImage
                ]
                []
            ]
        , div []
            [ Material.Options.styled Html.h2
                [ Material.Typography.title ]
                [ text "Relentlessness of the activity" ]
            , Material.Slider.view
                [ Material.Slider.onChange ChangeActivitySlider
                , Material.Slider.value <| toFloat activityEdit.sliderValue
                , Material.Slider.max 34
                , Material.Slider.min 1
                , Material.Slider.step 1
                ]
            , Material.Options.styled p
                [ Material.Typography.body1 ]
                [ text <| formatMinutes (minutesPerWeek activityEdit) ++ " per week." ]
            ]
        , div []
            [ Material.Button.render Mdl
                [ 0 ]
                mdl
                [ Material.Button.raised
                , Material.Button.colored
                , Material.Button.ripple
                , Material.Options.onClick SaveActivityType
                ]
                [ text "Save activity type" ]
            ]
        ]


locationView : Model -> Html Msg
locationView model =
    case model.location of
        Just Home ->
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
                        [ model.activities |> Activity.top model.time |> activitiesView model ]
                    ]
                ]

        Just Tomorrow ->
            div []
                [ Material.Options.styled Html.h1
                    [ Material.Typography.headline ]
                    [ text "To do tomorrow" ]
                , Material.Options.styled Html.h2
                    [ Material.Typography.subhead ]
                    [ text "Unless you do some of it today" ]
                ]

        Just Activities ->
            div [ class "fullpage" ]
                [ Material.Options.styled Html.h1
                    [ Material.Typography.headline ]
                    [ text "All your relentless activities" ]
                , Grid.grid []
                    [ Grid.cell
                        [ Grid.size Grid.Tablet 8, Grid.size Grid.Desktop 6, Grid.offset Grid.Desktop 3 ]
                        [ model.activities |> Activity.sorted |> activitiesView model ]
                    ]
                , div [ class "bottomright" ]
                    [ Material.Button.render Mdl
                        [ 0 ]
                        model.mdl
                        [ Material.Button.fab
                        , Material.Button.colored
                        , Material.Button.ripple
                        , Material.Options.onClick <| NewUrl "/new-activity"
                        ]
                        [ Material.Icon.i "add" ]
                    ]
                ]

        Just Log ->
            div []
                [ Material.Options.styled Html.h1
                    [ Material.Typography.headline ]
                    [ text "Activity log" ]
                ]

        Just NewActivity ->
            editActivityView model.mdl model.activityEdit

        Just (LogActivity id) ->
            case Activity.byId id model.activities of
                Just activity ->
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
                        , Material.Button.render Mdl
                            [ 0 ]
                            model.mdl
                            [ Material.Button.raised
                            , Material.Button.colored
                            , Material.Button.ripple
                            , Material.Options.onClick
                                ((if isRunning then
                                    StopActivity
                                  else
                                    StartActivity
                                 )
                                    activity.id
                                )
                            ]
                            [ text <|
                                if isRunning then
                                    "Stop activity"
                                else
                                    "Start activity"
                            ]
                        ]

                Nothing ->
                    div []
                        [ Material.Options.styled p
                            [ Material.Typography.headline ]
                            [ text "Unknown activity id" ]
                        ]

        Nothing ->
            div []
                [ Material.Options.styled p
                    [ Material.Typography.headline ]
                    [ text "Unknown page" ]
                ]


view : Model -> Html Msg
view model =
    Material.Layout.render Mdl
        model.mdl
        [ Material.Layout.fixedHeader
        , Material.Layout.fixedDrawer
        ]
        { header = [ pageHeader ]
        , drawer = drawer model.authenticatedData
        , tabs = ( [], [] )
        , main = [ viewBody model ]
        }


pageHeader : Html msg
pageHeader =
    Material.Layout.row []
        [ img [ class "logo", src "images/weather.svg" ] []
        , Material.Layout.title [] [ text "Relentless Groove" ]
        ]


drawer : AuthenticatedData -> List (Html Msg)
drawer authenticatedData =
    [ header [ class "drawer-header" ]
        [ img [ class "avatar", src authenticatedData.image_url ] []
        , div [ class "name" ] [ text authenticatedData.name ]
        ]
    , Material.Layout.navigation
        []
        [ Material.Layout.link
            [ Material.Options.onClick <| NewUrl "/" ]
            [ text "Today" ]
        , Material.Layout.link
            [ Material.Options.onClick <| NewUrl "/tomorrow" ]
            [ text "Tomorrow" ]
        , Material.Layout.link
            [ Material.Options.onClick <| NewUrl "/log" ]
            [ text "Log" ]
        , Material.Layout.link
            [ Material.Options.onClick <| NewUrl "/activities" ]
            [ text "Activities" ]
        , Material.Layout.link
            [ Material.Options.onClick <| NewUrl "/new-activity" ]
            [ text "New activity type" ]
        ]
    ]


signinView : AuthenticatedData -> Html msg
signinView authenticatedData =
    div
        [ id "my-signin2"
        , class <|
            if authenticatedData.id_token == "" then
                "visible"
            else
                "invisible"
        ]
        []


viewBody : Model -> Html Msg
viewBody model =
    div [ class "body-container fullpage" ]
        [ signinView model.authenticatedData
        , locationView model
        ]


postActivity : String -> Activity -> Cmd Msg
postActivity apiUrl activity =
    let
        body =
            activity
                |> toString
                |> Http.stringBody "application/json"
    in
    Http.request
        { method = "POST"
        , headers = []
        , url = apiUrl ++ "/activities"
        , body = body
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send PostActivityResult


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        activityEdit =
            model.activityEdit
    in
    case msg of
        Tick time ->
            ( { model | time = time }, Cmd.none )

        NewUrl url ->
            let
                layout =
                    model.mdl.layout

                newLayout =
                    { layout | isDrawerOpen = False }

                mdl =
                    model.mdl

                newMdl =
                    { mdl | layout = newLayout }
            in
            ( { model | mdl = newMdl }, Navigation.newUrl url )

        UrlChange path ->
            let
                location =
                    Url.parsePath route path

                cmd =
                    case location of
                        Just NewActivity ->
                            Random.generate NewActivityId (Random.int 0 Random.maxInt)

                        _ ->
                            Cmd.none
            in
            ( { model | location = location }, cmd )

        Mdl mdlmsg ->
            Material.update Mdl mdlmsg model

        NewActivityId id ->
            let
                activity =
                    model.activityEdit.activity
            in
            ( { model | activityEdit = { activityEdit | activity = { activity | id = id } } }, Cmd.none )

        ChangeActivityName name ->
            let
                activity =
                    model.activityEdit.activity
            in
            ( { model | activityEdit = { activityEdit | activity = { activity | name = name } } }, Cmd.none )

        ChangeActivityImage url ->
            let
                activity =
                    model.activityEdit.activity
            in
            ( { model | activityEdit = { activityEdit | activity = { activity | imageUrl = url } } }, Cmd.none )

        ChangeActivitySlider value ->
            ( { model | activityEdit = { activityEdit | sliderValue = round value } }, Cmd.none )

        SaveActivityType ->
            let
                updated =
                    updatedActivity model.activityEdit
            in
            ( { model
                | activities = updated :: model.activities
                , activityEdit = initActivityEdit
                , location = Just Activities
              }
            , postActivity model.apiUrl updated
            )

        PostActivityResult (Ok _) ->
            -- Not handling the results yet
            ( model, Cmd.none )

        PostActivityResult (Err failure) ->
            ( { model | errorMessage = "Error creating activity: " ++ toString failure }, Cmd.none )

        Authenticated data ->
            ( { model | authenticatedData = data }, Cmd.none )

        StartActivity id ->
            ( { model
                | activities = model.activities |> Activity.start id model.time
                , location = Just Activities
              }
            , Cmd.none
            )

        StopActivity id ->
            ( { model
                | activities = model.activities |> Activity.stop id model.time
                , location = Just Activities
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second Tick
        , authenticated Authenticated
        , Material.subscriptions Mdl model
        ]


main : Program String Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
