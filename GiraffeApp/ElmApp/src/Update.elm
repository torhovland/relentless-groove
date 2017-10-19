module Update exposing (update)

import Http
import Material
import Material.Layout as Layout
import Model exposing (Model, Msg)
import Navigation
import UrlParser as Url


postActivity : String -> Cmd Msg
postActivity apiUrl =
    let
        url =
            apiUrl ++ "/activities"

        body =
            Http.stringBody "application/json" "{ name: 'hallo', minutesPerWeek: 15 }"
    in
    Http.request
        { method = "POST"
        , headers = []
        , url = url
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

        Model.Increment ->
            ( { model | number = model.number + 1 }, Cmd.none )

        Model.Decrement ->
            ( { model | number = model.number - 1 }, Cmd.none )

        Model.PostActivity ->
            ( model, postActivity model.apiUrl )

        Model.PostActivityResult (Ok _) ->
            -- Not handling the results yet
            ( model, Cmd.none )

        Model.PostActivityResult (Err failure) ->
            ( { model | errorMessage = "Error creating activity: " ++ toString failure }, Cmd.none )

        Model.Authenticated data ->
            ( { model | authenticatedData = data }, Cmd.none )
