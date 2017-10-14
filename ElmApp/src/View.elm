module View exposing (view)

import Html exposing (Html, a, button, div, main_, nav, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, href, id)
import Html.Events exposing (onClick)
import Model exposing (Activity, Model, Msg)


activityView : Activity -> Html Msg
activityView activity =
    tr
        []
        [ td [ class "mdl-data-table__cell--non-numeric" ]
            [ text activity.name ]
        , td []
            [ text <| Model.remainingString activity ]
        , td []
            [ text <| Model.onScheduleRatioString activity ]
        ]


topActivitiesView : List Activity -> Html Msg
topActivitiesView activities =
    table [ class "mdl-data-table mdl-js-data-table mdl-data-table--selectable mdl-shadow--2dp" ]
        [ thead []
            [ tr []
                [ th [ class "mdl-data-table__cell--non-numeric" ]
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
    case Debug.log "location: " model.location of
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
    div [ class "mdl-layout mdl-js-layout mdl-layout--fixed-drawer" ]
        [ div [ class "mdl-layout__drawer" ]
            [ span [ class "mdl-layout-title" ]
                [ text model.authenticatedData.name ]
            , nav [ class "mdl-navigation" ]
                [ a [ class "mdl-navigation__link", onClick (Model.NewUrl "/") ]
                    [ text "Today" ]
                , a [ class "mdl-navigation__link", onClick (Model.NewUrl "/tomorrow") ]
                    [ text "Tomorrow" ]
                , a [ class "mdl-navigation__link", onClick (Model.NewUrl "/activities") ]
                    [ text "Activities" ]
                , a [ class "mdl-navigation__link", onClick (Model.NewUrl "/log") ]
                    [ text "Log" ]
                ]
            ]
        , main_ [ class "mdl-layout__content" ]
            [ div [ id "my-signin2" ]
                []
            , button [ onClick Model.PostActivity ] [ text "Post activity" ]
            , locationView model
            ]
        ]
