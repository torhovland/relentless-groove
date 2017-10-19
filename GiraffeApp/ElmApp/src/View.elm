module View exposing (view)

import Html exposing (Html, button, div, header, img, input, label, main_, p, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, for, id, name, src, type_, value)
import Html.Events exposing (onClick)
import Material.Button
import Material.Icon
import Material.Layout
import Material.Options
import Material.Textfield
import Material.Typography
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


newActivityView mdl newActivity =
    div []
        [ Material.Options.styled p
            [ Material.Typography.headline ]
            [ text "New activity type" ]
        , div []
            [ Material.Textfield.render Model.Mdl
                [ 0 ]
                mdl
                [ Material.Textfield.label "Name of activity type"
                , Material.Textfield.floatingLabel
                , Material.Textfield.value newActivity.name
                , Material.Options.onInput Model.ChangeNewActivityName
                ]
                []
            ]
        , div []
            [ Material.Button.render Model.Mdl
                [ 0 ]
                mdl
                [ Material.Button.raised
                , Material.Button.colored
                , Material.Button.ripple
                , Material.Options.onClick Model.SaveActivityType
                ]
                [ text "Save activity type" ]
            ]
        ]


locationView : Model -> Html Msg
locationView model =
    case model.location of
        Just Model.Home ->
            div []
                [ Material.Options.styled p
                    [ Material.Typography.headline ]
                    [ text "To do now" ]
                , Model.topActivities model.activities |> activitiesView
                ]

        Just Model.Tomorrow ->
            div []
                [ Material.Options.styled p
                    [ Material.Typography.headline ]
                    [ text "To do tomorrow" ]
                , Material.Options.styled p
                    [ Material.Typography.subhead ]
                    [ text "Unless you do some of it today" ]
                ]

        Just Model.Activities ->
            div [ class "fullpage" ]
                [ Material.Options.styled p
                    [ Material.Typography.headline ]
                    [ text "All your relentless activities" ]
                , Model.sortedActivities model.activities
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
            div []
                [ Material.Options.styled p
                    [ Material.Typography.headline ]
                    [ text "Activity log" ]
                ]

        Just Model.NewActivity ->
            newActivityView model.mdl model.newActivity

        Nothing ->
            div [] [ text "Unknown page" ]


view : Model -> Html Msg
view model =
    Material.Layout.render Model.Mdl
        model.mdl
        [ Material.Layout.fixedHeader
        , Material.Layout.fixedDrawer
        ]
        { header = pageHeader
        , drawer = drawer model.authenticatedData
        , tabs = ( [], [] )
        , main = [ viewBody model ]
        }


pageHeader =
    [ Material.Layout.row []
        [ img [ class "logo", src "images/weather.svg" ] []
        , Material.Layout.title [] [ text "Relentless Groove" ]
        ]
    ]


drawer : Model.AuthenticatedData -> List (Html Msg)
drawer authenticatedData =
    [ header [ class "drawer-header" ]
        [ img [ class "avatar", src authenticatedData.image_url ] []
        , div [ class "name" ] [ text authenticatedData.name ]
        ]
    , Material.Layout.navigation
        []
        [ Material.Layout.link
            [ Material.Options.onClick <| Model.NewUrl "/" ]
            [ text "Today" ]
        , Material.Layout.link
            [ Material.Options.onClick <| Model.NewUrl "/tomorrow" ]
            [ text "Tomorrow" ]
        , Material.Layout.link
            [ Material.Options.onClick <| Model.NewUrl "/log" ]
            [ text "Log" ]
        , Material.Layout.link
            [ Material.Options.onClick <| Model.NewUrl "/activities" ]
            [ text "Activities" ]
        , Material.Layout.link
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
        , locationView model
        ]
