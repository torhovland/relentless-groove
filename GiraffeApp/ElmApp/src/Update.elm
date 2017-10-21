module Update exposing (update)

import Http
import Material
import Model exposing (Model, Msg)
import Navigation
import Random
import UrlParser as Url


postActivity : String -> Model.Activity -> Cmd Msg
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
        |> Http.send Model.PostActivityResult


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        activityEdit =
            model.activityEdit
    in
    case msg of
        Model.NewUrl url ->
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

        Model.UrlChange path ->
            let
                location =
                    Url.parsePath Model.route path

                cmd =
                    case location of
                        Just Model.NewActivity ->
                            Random.generate Model.NewActivityId (Random.int 0 Random.maxInt)

                        _ ->
                            Cmd.none
            in
            ( { model | location = location }, cmd )

        Model.Mdl mdlmsg ->
            Material.update Model.Mdl mdlmsg model

        Model.NewActivityId id ->
            let
                activity =
                    model.activityEdit.activity
            in
            ( { model | activityEdit = { activityEdit | activity = { activity | id = id } } }, Cmd.none )

        Model.ChangeActivityName name ->
            let
                activity =
                    model.activityEdit.activity
            in
            ( { model | activityEdit = { activityEdit | activity = { activity | name = name } } }, Cmd.none )

        Model.ChangeActivityImage url ->
            let
                activity =
                    model.activityEdit.activity
            in
            ( { model | activityEdit = { activityEdit | activity = { activity | imageUrl = url } } }, Cmd.none )

        Model.ChangeActivitySlider value ->
            ( { model | activityEdit = { activityEdit | sliderValue = round value } }, Cmd.none )

        Model.SaveActivityType ->
            let
                updatedActivity =
                    Model.updatedActivity model.activityEdit
            in
            ( { model
                | activities = updatedActivity :: model.activities
                , activityEdit = Model.initActivityEdit
                , location = Just Model.Activities
              }
            , postActivity model.apiUrl updatedActivity
            )

        Model.PostActivityResult (Ok _) ->
            -- Not handling the results yet
            ( model, Cmd.none )

        Model.PostActivityResult (Err failure) ->
            ( { model | errorMessage = "Error creating activity: " ++ toString failure }, Cmd.none )

        Model.Authenticated data ->
            ( { model | authenticatedData = data }, Cmd.none )
