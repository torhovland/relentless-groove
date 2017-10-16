module View exposing (view)

import Html exposing (Html, a, button, div, h1, main_, nav, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Material
import Material.Layout
import Material.Options exposing (css)
import Material.Scheme
import Model exposing (Activity, Model, Msg)


type alias Mdl =
    Material.Model


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
    Material.Layout.render Model.Mdl
        model.mdl
        [ Material.Layout.fixedHeader
        , Material.Layout.fixedDrawer
        ]
        { header =
            [ Material.Layout.row []
                [ Material.Layout.title [] [ text "Relentless Groove" ] ]
            ]
        , drawer = [ text "Drawer" ]
        , tabs = ( [], [] )
        , main = [ viewBody model ]
        }


viewBody : Model -> Html Msg
viewBody model =
    main_ []
        [ div [ id "my-signin2" ]
            []
        , button [ onClick Model.PostActivity ] [ text "Post activity" ]
        , locationView model
        ]
