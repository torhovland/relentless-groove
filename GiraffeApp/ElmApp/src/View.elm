module View exposing (view)

import Html exposing (Html, div, header, img, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, id, src)
import Html.Events exposing (onClick)
import Material
import Material.Button
import Material.Grid as Grid
import Material.Icon
import Material.Layout
import Material.List
import Material.Options
import Material.Slider
import Material.Textfield
import Material.Typography
import Model exposing (Activity, Model, Msg)


activityView : Model -> Activity -> Html Msg
activityView model activity =
    Material.List.li [ Material.List.withSubtitle ]
        [ Material.List.content
            [ Material.Options.attribute <|
                Html.Events.onClick <|
                    Model.NewUrl <|
                        "/log-activity/"
                            ++ toString activity.id
            ]
            [ Material.List.avatarImage activity.imageUrl []
            , text activity.name
            , Material.List.subtitle [] [ text <| Model.remainingString model activity ++ " to do" ]
            ]
        , text <| Model.onScheduleRatioString model activity
        ]


activitiesView : Model -> List Activity -> Html Msg
activitiesView model activityList =
    Material.List.ul [] <| List.map (activityView model) activityList


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
                [ text <| Model.formatMinutes (Model.minutesPerWeek activityEdit) ++ " per week." ]
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
                [ Grid.grid []
                    [ Grid.cell
                        [ Grid.size Grid.Tablet 8, Grid.size Grid.Desktop 6, Grid.offset Grid.Desktop 3 ]
                        [ Material.Options.styled Html.h4 [ Material.Typography.headline ] [ text "To do now" ]
                        , Model.topActivities model |> activitiesView model
                        ]
                    ]
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
                [ Grid.grid []
                    [ Grid.cell
                        [ Grid.size Grid.Tablet 8, Grid.size Grid.Desktop 6, Grid.offset Grid.Desktop 3 ]
                        [ Material.Options.styled Html.h4 [ Material.Typography.headline ] [ text "All your relentless activities" ]
                        , Model.sortedActivities model.activities |> activitiesView model
                        ]
                    ]
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

        Just (Model.LogActivity id) ->
            let
                maybeActivity =
                    Model.activityById id model.activities
            in
            case maybeActivity of
                Just activity ->
                    div []
                        [ Material.Options.styled p
                            [ Material.Typography.headline ]
                            [ text "Log time on activity" ]
                        , Material.Options.styled p
                            [ Material.Typography.subhead ]
                            [ text <| activity.name ]
                        , Material.Button.render Model.Mdl
                            [ 0 ]
                            model.mdl
                            [ Material.Button.raised
                            , Material.Button.colored
                            , Material.Button.ripple
                            , Material.Options.onClick (Model.StartActivity activity.id)
                            ]
                            [ text "Start activity" ]
                        ]

                Nothing ->
                    div []
                        [ Material.Options.styled p
                            [ Material.Typography.headline ]
                            [ text "Unknown activity id" ]
                        ]

        Nothing ->
            div []
                [ Material.Options.styled p
                    [ Material.Typography.headline ]
                    [ text "Unknown page" ]
                ]


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
