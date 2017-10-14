module View exposing (view)

import Html exposing (Html, a, button, div, main_, nav, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, href, id)
import Html.Events exposing (onClick)
import Model exposing (..)


onScheduleSort a b =
    compare (onScheduleRatio a) (onScheduleRatio b)


topActivities : List Activity -> List Activity
topActivities activities =
    activities
        |> List.sortWith onScheduleSort
        |> List.take 3


activityView : Activity -> Html Msg
activityView activity =
    tr
        []
        [ td [ class "mdl-data-table__cell--non-numeric" ]
            [ text activity.name ]
        , td []
            [ text (formatTime (remaining activity)) ]
        , td []
            [ text (formatPercent (onScheduleRatio activity)) ]
        ]


topActivitiesView model =
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
        , tbody [] (List.map activityView (topActivities model.activities))
        ]


view : Model -> Html Msg
view model =
    div [ class "mdl-layout mdl-js-layout mdl-layout--fixed-drawer" ]
        [ div [ class "mdl-layout__drawer" ]
            [ span [ class "mdl-layout-title" ]
                [ text model.authenticatedData.name ]
            , nav [ class "mdl-navigation" ]
                [ a [ class "mdl-navigation__link", href "" ]
                    [ text "Link" ]
                , a [ class "mdl-navigation__link", href "" ]
                    [ text "Link" ]
                , a [ class "mdl-navigation__link", href "" ]
                    [ text "Link" ]
                , a [ class "mdl-navigation__link", href "" ]
                    [ text "Link" ]
                ]
            ]
        , main_ [ class "mdl-layout__content" ]
            [ div [ id "my-signin2" ]
                []
            , button [ onClick PostActivity ] [ text "Post activity" ]
            , topActivitiesView model
            ]
        ]
