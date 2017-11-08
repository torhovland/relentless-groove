port module Main exposing (main)

import Activity exposing (Activity, ActivityEdit, minutesPerWeek)
import Http
import Material
import Model
    exposing
        ( AuthenticatedData
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
        )
import Navigation
import Random
import Time
import UrlParser as Url exposing ((</>))
import View


port authenticated : (AuthenticatedData -> msg) -> Sub msg


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
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }
