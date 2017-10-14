module Update exposing (update)

import Http
import Model exposing (Model, Msg)


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
        Model.UrlChange _ ->
            -- Not supporting change of url at the moment, just return the model unchanged, with no side effect.
            ( model, Cmd.none )

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
