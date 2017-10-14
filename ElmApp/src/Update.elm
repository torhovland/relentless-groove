module Update exposing (update)

import Http
import Model exposing (..)


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
        |> Http.send PostActivityResult


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange url ->
            -- Not supporting change of url at the moment, just return the model unchanged, with no side effect.
            ( model, Cmd.none )

        Increment ->
            ( { model | number = model.number + 1 }, Cmd.none )

        Decrement ->
            ( { model | number = model.number - 1 }, Cmd.none )

        PostActivity ->
            ( model, postActivity model.apiUrl )

        PostActivityResult (Ok statistics) ->
            ( model, Cmd.none )

        PostActivityResult (Err failure) ->
            let
                log =
                    Debug.log "error" failure
            in
            ( model, Cmd.none )

        Authenticated msg ->
            ( { model | authenticatedData = Debug.log "authenticated: " msg }, Cmd.none )
