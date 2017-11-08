module Model
    exposing
        ( AuthenticatedData
        , Model
        , Msg
            ( Authenticated
            , ChangeActivityImage
            , ChangeActivityName
            , ChangeActivitySlider
            , Mdl
            , NewActivityId
            , NewUrl
            , PostActivityResult
            , SaveActivityType
            , StartActivity
            , StopActivity
            , Tick
            , UrlChange
            )
        , Route(Activities, Home, Log, LogActivity, NewActivity, Tomorrow)
        )

import Activity exposing (Activity, ActivityEdit)
import Http
import Material
import Navigation
import Time exposing (Time)


type Route
    = Home
    | Tomorrow
    | Log
    | Activities
    | NewActivity
    | LogActivity Int


type alias AuthenticatedData =
    { name : String
    , image_url : String
    , id_token : String
    }


type alias Model =
    { time : Time
    , location : Maybe Route
    , apiUrl : String
    , authenticatedData : AuthenticatedData
    , errorMessage : String
    , mdl : Material.Model
    , activities : List Activity
    , activityEdit : ActivityEdit
    }


type Msg
    = Tick Time
    | NewUrl String
    | UrlChange Navigation.Location
    | Authenticated AuthenticatedData
    | Mdl (Material.Msg Msg)
    | NewActivityId Int
    | ChangeActivityName String
    | ChangeActivityImage String
    | ChangeActivitySlider Float
    | SaveActivityType
    | PostActivityResult (Result Http.Error ())
    | StartActivity Int
    | StopActivity Int
