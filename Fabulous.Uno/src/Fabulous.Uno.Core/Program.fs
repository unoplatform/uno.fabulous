// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.

namespace Fabulous.Uno

open Fabulous
open Windows.UI.Xaml
open Windows.UI.Core
open Microsoft.Extensions.Logging
open Windows.UI.Xaml.Controls

type UnoHost(app: Application) =
    interface IHost with
        member __.GetRootView() =
            match Window.Current.Content with
            | null -> failwith "No root view"
            | rootView -> (rootView :?> Frame).Content

        member __.SetRootView(rootView) =
            match rootView with
            | :? FrameworkElement as element -> (Window.Current.Content :?> Frame).Content <- element
            | _ -> failwithf "Incorrect model type: expected a FrameworkElement but got a %O" (rootView.GetType())


/// Program module - functions to manipulate program instances
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module UnoProgram =    
    let private syncDispatch (dispatch: 'msg -> unit) =
        fun msg ->
            Window.Current.Dispatcher.RunAsync(CoreDispatcherPriority.Normal, fun () -> dispatch msg) |> ignore
            ()
            
    let private syncAction (fn: unit -> unit) =
        fun () ->
            Window.Current.Dispatcher.RunAsync(CoreDispatcherPriority.Normal, fun () -> fn()) |> ignore
            ()

    let configureFilters (factory: ILoggerFactory) =
        let settings = new FilterLoggerSettings()
        settings.Add("Uno", LogLevel.Warning);
        settings.Add("Windows", LogLevel.Warning);

        factory
            .WithFilter(settings)
            .AddConsole(LogLevel.Debug)

    let runWith app arg program =
        let host = UnoHost(app)

        typeof<Uno.UI.Wasm.WasmHttpHandler>.ToString() |> ignore

        configureFilters Uno.Extensions.LogExtensionPoint.AmbientLoggerFactory |> ignore

        let rootWindow = new Frame()
        rootWindow.Background <- SolidColorBrushHelper.Black

        Window.Current.Content <- rootWindow
        Window.Current.Activate()
        
        program
        |> Program.withCanReuseView ViewHelpers.canReuseView
        |> Program.withSyncDispatch syncDispatch
        |> Program.withSyncAction syncAction
        |> Program.runWithFabulous host arg
        |> ignore
        
    let run app program =
        runWith app () program