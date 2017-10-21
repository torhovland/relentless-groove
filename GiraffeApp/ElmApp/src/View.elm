module View exposing (view)

import Html exposing (Html, div, header, img, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, id, src)
import Material
import Material.Button
import Material.Icon
import Material.Layout
import Material.Options
import Material.Slider
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


activitiesView : List Activity -> Html Msg
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


editActivityView : Material.Model -> Model.ActivityEdit -> Html Msg
editActivityView mdl activityEdit =
    div []
        [ div []
            [ Material.Options.styled p
                [ Material.Typography.headline ]
                [ text "New activity type" ]
            , Material.Options.styled p
                [ Material.Typography.title ]
                [ text "Activity details" ]
            , Material.Textfield.render Model.Mdl
                [ 0 ]
                mdl
                [ Material.Textfield.label "Name of activity type"
                , Material.Textfield.floatingLabel
                , Material.Textfield.value activityEdit.activity.name
                , Material.Options.onInput Model.ChangeActivityName
                ]
                []
            ]
        , div []
            [ Material.Textfield.render Model.Mdl
                [ 0 ]
                mdl
                [ Material.Textfield.label "URL to activity icon"
                , Material.Textfield.floatingLabel
                , Material.Textfield.value activityEdit.activity.imageUrl
                , Material.Options.onInput Model.ChangeActivityImage
                ]
                []
            ]
        , div []
            [ Material.Options.styled p
                [ Material.Typography.title ]
                [ text "Relentlessness of the activity" ]
            , Material.Slider.view
                [ Material.Slider.onChange Model.ChangeActivitySlider
                , Material.Slider.value <| toFloat activityEdit.sliderValue
                , Material.Slider.max 34
                , Material.Slider.min 1
                , Material.Slider.step 1
                ]
            , Material.Options.styled p
                [ Material.Typography.body1 ]
                [ text <| formatMinutes (Model.minutesPerWeek activityEdit) ++ " per week." ]
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
            editActivityView model.mdl model.activityEdit

        Nothing ->
            div [] [ text "Unknown page" ]


view : Model -> Html Msg
view model =
    Material.Layout.render Model.Mdl
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


signinView : Model.AuthenticatedData -> Html msg
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
