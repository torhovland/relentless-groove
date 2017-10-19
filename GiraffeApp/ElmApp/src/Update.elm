module Update exposing (update)

import Http
import Material
import Model exposing (Model, Msg)
import Navigation
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

        Model.UrlChange location ->
            ( { model | location = Url.parsePath Model.route location }, Cmd.none )

        Model.Mdl mdlmsg ->
            Material.update Model.Mdl mdlmsg model

        Model.ChangeNewActivityName name ->
            let
                newActivity =
                    model.newActivity

                updatedActivity =
                    { newActivity | name = name }
            in
            ( { model | newActivity = updatedActivity }, Cmd.none )

        Model.SaveActivityType ->
            ( { model
                | activities = model.newActivity :: model.activities
                , newActivity = Model.initActivity
              }
            , postActivity model.apiUrl model.newActivity
            )

        Model.Increment ->
            ( { model | number = model.number + 1 }, Cmd.none )

        Model.Decrement ->
            ( { model | number = model.number - 1 }, Cmd.none )

        Model.PostActivityResult (Ok _) ->
            -- Not handling the results yet
            ( model, Cmd.none )

        Model.PostActivityResult (Err failure) ->
            ( { model | errorMessage = "Error creating activity: " ++ toString failure }, Cmd.none )

        Model.Authenticated data ->
            ( { model | authenticatedData = data }, Cmd.none )
