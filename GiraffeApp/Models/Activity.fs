namespace GiraffeApp.Models

open System
open Microsoft.WindowsAzure.Storage.Table

[<CLIMutable>]
type ActivityType =
    {
        Name : string
        MinutesPerWeek : int
    }

type ActivityEntity() =
    inherit TableEntity()
    member val Name = "" with get, set
    member val MinutesPerWeek = 0 with get, set

module Activity =
    let toEntity userId activity : ActivityEntity =
        let entity = ActivityEntity()
        entity.PartitionKey <- userId
        entity.RowKey <- Guid.NewGuid().ToString()
        entity.Name <- activity.Name
        entity.MinutesPerWeek <- activity.MinutesPerWeek
        entity
