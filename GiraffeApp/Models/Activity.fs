namespace GiraffeApp.Models

open System

[<CLIMutable>]
type Activity =
    {
        Name           : string
        MinutesPerWeek : int
    }