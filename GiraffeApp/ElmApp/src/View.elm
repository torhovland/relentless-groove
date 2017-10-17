module View exposing (view)

import Html exposing (Html, button, div, header, img, main_, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, id, src)
import Html.Events exposing (onClick)
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


topActivitiesView : List Activity -> Html Msg
topActivitiesView activities =
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
        , tbody [] (List.map activityView (Model.topActivities activities))
        ]


locationView : Model -> Html Msg
locationView model =
    case model.location of
        Just Model.Home ->
            topActivitiesView model.activities

        Just Model.Tomorrow ->
            div [] [ text "Tomorrow" ]

        Just Model.Activities ->
            div [] [ text "Activities" ]

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
            [ Material.Options.onClick (Model.NewUrl "/") ]
            [ text "Today" ]
        , Layout.link
            [ Material.Options.onClick (Model.NewUrl "/tomorrow") ]
            [ text "Tomorrow" ]
        , Layout.link
            [ Material.Options.onClick (Model.NewUrl "/activities") ]
            [ text "Activities" ]
        , Layout.link
            [ Material.Options.onClick (Model.NewUrl "/log") ]
            [ text "Log" ]
        ]
    ]


viewBody : Model -> Html Msg
viewBody model =
    main_ []
        [ div [ id "my-signin2" ]
            []
        , button [ onClick Model.PostActivity ] [ text "Post activity" ]
        , locationView model
        ]
