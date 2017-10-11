module GiraffeApp.App

open System
open System.IO
open System.Collections.Generic
open System.Threading.Tasks
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.Authentication.JwtBearer
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.IdentityModel.Tokens
open Microsoft.WindowsAzure.Storage
open Microsoft.WindowsAzure.Storage.Table
open Giraffe.Tasks
open Giraffe.HttpHandlers
open Giraffe.HttpContextExtensions
open Giraffe.Middleware
open Giraffe.Razor.HttpHandlers
open Giraffe.Razor.Middleware
open GiraffeApp.Models

let authorize =
    requiresAuthentication (challenge JwtBearerDefaults.AuthenticationScheme)
    
let postActivity =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let! activity = ctx.BindJson<Activity>()
            let configuration = ctx.GetService<IConfiguration>()
            let connectionString = configuration.["TableStorage"]
            let storageAccount = CloudStorageAccount.Parse(connectionString);
            let message = sprintf "Posted activity %s with %i minutes per week. Will save to %s." activity.Name activity.MinutesPerWeek connectionString
            return! text message next ctx
        }

// ---------------------------------
// Web app
// ---------------------------------

let webApp =
    choose [
        POST >=>
            choose [
                route "/api/v1/activities" >=> authorize >=> postActivity
            ]
        GET >=>
            choose [
                route "/" >=> text "Hello world, from Giraffe!"
            ]
        setStatusCode 404 >=> text "Not Found"
    ]


// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder : CorsPolicyBuilder) =
    builder.WithOrigins("http://localhost:8080").AllowAnyMethod().AllowAnyHeader() |> ignore

let configureApp (app : IApplicationBuilder) =
    app.UseAuthentication() |> ignore
    app.UseCors(configureCors) |> ignore
    app.UseGiraffeErrorHandler errorHandler
    app.UseStaticFiles() |> ignore
    app.UseGiraffe webApp

let authenticationOptions (o : AuthenticationOptions) =
    o.DefaultAuthenticateScheme <- JwtBearerDefaults.AuthenticationScheme
    o.DefaultChallengeScheme <- JwtBearerDefaults.AuthenticationScheme

let jwtBearerOptions (cfg : JwtBearerOptions) =
    cfg.SaveToken <- true
    cfg.IncludeErrorDetails <- true
    cfg.Authority <- "https://accounts.google.com"
    cfg.Audience <- "471287974858-e1pgspg0nctdecdbb1jtrjldspb9vsqu.apps.googleusercontent.com"
    cfg.TokenValidationParameters <- TokenValidationParameters (
        ValidIssuer = "accounts.google.com"
    )

let configureServices (services : IServiceCollection) =
    let sp  = services.BuildServiceProvider()
    let env = sp.GetService<IHostingEnvironment>()
    let viewsFolderPath = Path.Combine(env.ContentRootPath, "Views")
    services.AddRazorEngine viewsFolderPath |> ignore
    services.AddCors |> ignore
    services.AddAuthentication(authenticationOptions)
        .AddJwtBearer(Action<JwtBearerOptions> jwtBearerOptions) |> ignore

[<EntryPoint>]
let main argv =
    WebHost.CreateDefaultBuilder()
        .ConfigureServices(configureServices)
        .Configure(Action<IApplicationBuilder> configureApp)
        .Build()
        .Run()
    0