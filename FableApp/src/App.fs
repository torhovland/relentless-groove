module FableApp

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Helpers.React.Props
module R = Fable.Helpers.React
open Elmish
open Elmish.React

let logAuth : string -> obj option = import "logAuth" "./google-auth.js"

type Model = int

type Msg = Increment | Decrement
    
let init () =
    0

let view model dispatch =
  R.div []
      [ R.button [ OnClick (fun _ -> dispatch Decrement) ] [ R.str "-" ]
        R.div [] [ R.str (sprintf "%A" model) ]
        R.button [ OnClick (fun _ -> dispatch Increment) ] [ R.str "+" ] ]

let update (msg:Msg) count =
  match msg with
  | Increment -> count + 1
  | Decrement -> count - 1

Program.mkSimple init update view 
|> Program.withReact "elmish-app"
|> Program.run