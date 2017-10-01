module FableApp

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Helpers.React.Props
module R = Fable.Helpers.React
open Elmish
open Elmish.React

let logAuth : unit -> obj option = import "logAuth" "./google-auth.js"

type Model = int

type Msg = 
  | Failure of string
  | Increment 
  | Decrement 
  | LogAuth 
    
let init () : Model * Cmd<Msg> =
    0, Cmd.none

let view model dispatch =
  R.div []
      [ R.button [ OnClick (fun _ -> dispatch Decrement) ] [ R.str "-" ]
        R.div [] [ R.str (sprintf "%A" model) ]
        R.button [ OnClick (fun _ -> dispatch Increment) ] [ R.str "+" ]
        R.button [ OnClick (fun _ -> dispatch LogAuth) ] [ R.str "Log auth" ] ]

let doLogAuth _ =
  logAuth () |> ignore
  ()

let update (msg : Msg) (count : Model) =
  match msg with
  | Failure _ -> count, Cmd.none
  | Increment -> count + 1, Cmd.none
  | Decrement -> count - 1, Cmd.none
  | LogAuth -> count, Cmd.attemptFunc doLogAuth () (string >> Failure)

Program.mkProgram init update view 
|> Program.withReact "elmish-app"
|> Program.run