port module Main exposing (main)

import Activity exposing (Activity)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Http
import Material
import Material.Helpers
import Material.Layout
import Material.Snackbar
import Navigation
import Random
import Time exposing (Time)
import UrlParser as Url exposing ((</>))
import View


port authenticated : (AuthenticatedData -> msg) -> Sub msg


type Route
    = Home
    | Tomorrow
    | Log
    | Activities
    | NewActivity
    | LogActivity Int


type alias AuthenticatedData =
    { name : String
    , imageUrl : String
    , idToken : String
    }


type alias ActivityEdit =
    { activity : Activity
    , sliderValue : Int
    }


type alias Model =
    { time : Time
    , authenticatedData : AuthenticatedData
    , mdl : Material.Model
    , apiUrl : String
    , location : Maybe Route
    , snackbar : Material.Snackbar.Model ()
    , activities : List Activity
    , activityEdit : ActivityEdit
    }


type Msg
    = Tick Time
    | Authenticated AuthenticatedData
    | Mdl (Material.Msg Msg)
    | Snackbar (Material.Snackbar.Msg ())
    | NewUrl String
    | UrlChange Navigation.Location
    | NewActivityId Int
    | ChangeActivityName String
    | ChangeActivityImage String
    | ChangeActivitySlider Float
    | SaveActivityType
    | PostActivityResult (Result Http.Error ())
    | StartActivity Int
    | StopActivity Int


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


initActivityEdit : ActivityEdit
initActivityEdit =
    ActivityEdit Activity.initActivity 3


init : String -> Navigation.Location -> ( Model, Cmd Msg )
init apiUrl location =
    ( { time = 0
      , location = Url.parsePath route location
      , apiUrl = apiUrl
      , authenticatedData = AuthenticatedData "" "" ""
      , mdl = Material.model
      , snackbar = Material.Snackbar.model
      , activities = Activity.initActivities
      , activityEdit = initActivityEdit
      }
    , Material.init Mdl
    )


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


minutesPerWeek : Int -> Int
minutesPerWeek slider =
    if slider <= 12 then
        slider * 5
    else if slider <= 20 then
        12 * 5 + (slider - 12) * 15
    else
        12 * 5 + 8 * 15 + (slider - 20) * 30


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        activityEdit =
            model.activityEdit

        updateActivityEdit model activityEdit =
            { model | activityEdit = activityEdit }
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

        Mdl materialMsg ->
            Material.update Mdl materialMsg model

        -- Need to set activityId here, as a callback from Random.generate
        NewActivityId activityId ->
            let
                activity =
                    activityEdit.activity

                updatedActivityEdit =
                    { activityEdit | activity = { activity | id = activityId } }
            in
            ( updateActivityEdit model updatedActivityEdit, Cmd.none )

        ChangeActivityName name ->
            let
                activity =
                    activityEdit.activity

                updatedActivityEdit =
                    { activityEdit | activity = { activity | name = name } }
            in
            ( updateActivityEdit model updatedActivityEdit, Cmd.none )

        ChangeActivityImage url ->
            let
                activity =
                    activityEdit.activity

                updatedActivityEdit =
                    { activityEdit | activity = { activity | imageUrl = url } }
            in
            ( updateActivityEdit model updatedActivityEdit, Cmd.none )

        ChangeActivitySlider value ->
            let
                sliderValue =
                    round value

                activity =
                    activityEdit.activity

                updatedActivityEdit =
                    { activityEdit
                        | sliderValue = sliderValue
                        , activity = { activity | minutesPerWeek = minutesPerWeek sliderValue }
                    }
            in
            ( updateActivityEdit model updatedActivityEdit, Cmd.none )

        SaveActivityType ->
            let
                updated =
                    model.activityEdit.activity
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
            let
                ( snackbar, snackbarCmd ) =
                    Material.Snackbar.add (Material.Snackbar.toast () ("Error saving activity: " ++ toString failure)) model.snackbar
                        |> Material.Helpers.map2nd (Cmd.map Snackbar)
            in
            ( { model | snackbar = snackbar }, snackbarCmd )

        Authenticated data ->
            ( { model | authenticatedData = data }, Cmd.none )

        Snackbar msg_ ->
            Material.Snackbar.update msg_ model.snackbar
                |> Material.Helpers.map1st (\s -> { model | snackbar = s })
                |> Material.Helpers.map2nd (Cmd.map Snackbar)

        StartActivity activityId ->
            ( { model
                | activities = model.activities |> Activity.start activityId model.time
                , location = Just Activities
              }
            , Cmd.none
            )

        StopActivity activityId ->
            ( { model
                | activities = model.activities |> Activity.stop activityId model.time
                , location = Just Activities
              }
            , Cmd.none
            )


locationView : Model -> Html Msg
locationView model =
    case model.location of
        Just Home ->
            View.homePageView model.time (model.activities |> Activity.top model.time)

        Just Tomorrow ->
            View.tomorrowPageView

        Just Activities ->
            View.activitiesPageView
                Mdl
                NewUrl
                model.mdl
                model.time
                model.activities

        Just Log ->
            View.logPageView

        Just NewActivity ->
            View.editActivityPageView
                Mdl
                ChangeActivityName
                ChangeActivityImage
                ChangeActivitySlider
                SaveActivityType
                model.mdl
                model.activityEdit.activity
                (toFloat model.activityEdit.sliderValue)

        Just (LogActivity activityId) ->
            case Activity.byId activityId model.activities of
                Just activity ->
                    View.logActivityPageView Mdl StartActivity StopActivity model.mdl activity

                Nothing ->
                    View.unknownActivityPageView

        Nothing ->
            View.unknownPageView


view : Model -> Html Msg
view model =
    let
        auth =
            model.authenticatedData
    in
    Material.Layout.render Mdl
        model.mdl
        [ Material.Layout.fixedHeader
        , Material.Layout.fixedDrawer
        ]
        { header = [ View.pageHeader ]
        , drawer = View.drawer NewUrl auth.name auth.imageUrl
        , tabs = ( [], [] )
        , main =
            [ div [ class "body-container fullpage" ]
                [ View.signinView <| auth.idToken == ""
                , locationView model
                , Material.Snackbar.view model.snackbar |> Html.map Snackbar
                ]
            ]
        }


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
