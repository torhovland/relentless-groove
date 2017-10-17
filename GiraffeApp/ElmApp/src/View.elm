module View exposing (view)

import Html exposing (Html, button, div, header, img, main_, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, id, src)
import Html.Events exposing (onClick)
import Material.Button
import Material.Icon
import Material.Layout as Layout
import Material.Options
import Model exposing (Activity, Model, Msg)


activityView : Activity -> Html Msg
activityView activity =
    tr
        []
        [ td []
            [ text activity.name ]
        , td []
            [ text <| Model.remainingString activity ]
        , td []
            [ text <| Model.onScheduleRatioString activity ]
        ]


activitiesView activityList =
    table []
        [ thead []
            [ tr []
                [ th []
                    [ text "Activity" ]
                , th []
                    [ text "Remaining" ]
                , th []
                    [ text "On schedule" ]
                ]
            ]
        , tbody [] <| List.map activityView activityList
        ]


locationView : Model -> Html Msg
locationView model =
    case model.location of
        Just Model.Home ->
            Model.topActivities model.activities |> activitiesView

        Just Model.Tomorrow ->
            div [] [ text "Tomorrow" ]

        Just Model.Activities ->
            div [ class "fullpage" ]
                [ Model.sortedActivities model.activities
                    |> activitiesView
                , div [ class "bottomright" ]
                    [ Material.Button.render Model.Mdl
                        [ 0 ]
                        model.mdl
                        [ Material.Button.fab
                        , Material.Button.colored
                        , Material.Button.ripple
                        , Material.Options.onClick <| Model.NewUrl "/new-activity"
                        ]
                        [ Material.Icon.i "add" ]
                    ]
                ]

        Just Model.Log ->
            div [] [ text "Log" ]

        Nothing ->
            div [] [ text "Unknown page" ]


view : Model -> Html Msg
view model =
    Layout.render Model.Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.fixedDrawer
        ]
        { header = pageHeader
        , drawer = drawer model.authenticatedData
        , tabs = ( [], [] )
        , main = [ viewBody model ]
        }


pageHeader =
    [ Layout.row []
        [ img [ class "logo", src "images/weather.svg" ] []
        , Layout.title [] [ text "Relentless Groove" ]
        ]
    ]


drawer : Model.AuthenticatedData -> List (Html Msg)
drawer authenticatedData =
    [ header [ class "drawer-header" ]
        [ img [ class "avatar", src authenticatedData.image_url ] []
        , div [ class "name" ] [ text authenticatedData.name ]
        ]
    , Layout.navigation
        []
        [ Layout.link
            [ Material.Options.onClick <| Model.NewUrl "/" ]
            [ text "Today" ]
        , Layout.link
            [ Material.Options.onClick <| Model.NewUrl "/tomorrow" ]
            [ text "Tomorrow" ]
        , Layout.link
            [ Material.Options.onClick <| Model.NewUrl "/log" ]
            [ text "Log" ]
        , Layout.link
            [ Material.Options.onClick <| Model.NewUrl "/activities" ]
            [ text "Activities" ]
        , Layout.link
            [ Material.Options.onClick <| Model.NewUrl "/new-activity" ]
            [ text "New activity type" ]
        ]
    ]


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
        , button [ onClick Model.PostActivity ] [ text "Post activity" ]
        , locationView model
        ]
