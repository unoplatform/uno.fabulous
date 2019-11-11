// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace CounterApp

open Fabulous
open Fabulous.Uno
open System.Diagnostics
open Windows.UI.Xaml
open Windows.UI.Xaml.Controls

module App = 
    type Model = 
      { Count : int 
        Step : int
        TimerOn: bool }

    type Msg = 
        | Increment 
        | Decrement 
        | Reset
        | SetStep of int
        | TimerToggled of bool
        | TimedTick

    type CmdMsg =
        | TickTimer

    let timerCmd () =
        async { do! Async.Sleep 200
                return TimedTick }
        |> Cmd.ofAsyncMsg

    let mapCmdMsgToCmd cmdMsg =
        match cmdMsg with
        | TickTimer -> timerCmd()

    let initModel () = { Count = 0; Step = 1; TimerOn=false }

    let init () = initModel () , []

    let update msg model =
        match msg with
        | Increment -> { model with Count = model.Count + model.Step }, []
        | Decrement -> { model with Count = model.Count - model.Step }, []
        | Reset -> init ()
        | SetStep n -> { model with Step = n }, []
        | TimerToggled on -> { model with TimerOn = on }, (if on then [ TickTimer ] else [])
        | TimedTick -> if model.TimerOn then { model with Count = model.Count + model.Step }, [ TickTimer ] else model, [] 

    let view (model: Model) dispatch =  
          View.StackPanel(padding = Thickness 30.0, verticalAlignment = VerticalAlignment.Center,
            children=[
              View.TextBlock(text=sprintf "%d" model.Count, horizontalAlignment=HorizontalAlignment.Center, width=200.0, horizontalTextAlignment=TextAlignment.Center)
              View.Button(content="Increment", command= (fun () -> dispatch Increment))
              View.Button(content="Decrement", command= (fun () -> dispatch Decrement)) 
              View.TextBlock(text=sprintf "Step size: %d" model.Step, horizontalAlignment=HorizontalAlignment.Center)
              View.Button(content="Reset", horizontalAlignment=HorizontalAlignment.Center, command=(fun () -> dispatch Reset), commandCanExecute = (model <> initModel () ))
            ])
             
    let program = 
        Program.mkProgramWithCmdMsg init update view mapCmdMsgToCmd


type CounterApp () as app = 
    inherit Application ()

    override u.OnLaunched activatedArgs =
        Windows.UI.Xaml.GenericStyles.Initialize()
        Windows.ApplicationModel.Resources.ResourceLoader.DefaultLanguage <- "en-US"
        Windows.ApplicationModel.Resources.ResourceLoader.AddLookupAssembly(System.Reflection.Assembly.Load("Uno.UI, Version=255.255.255.255, Culture=neutral, PublicKeyToken=null"))

        App.program
        |> Program.withConsoleTrace
        |> UnoProgram.run app


#if DEBUG
    // Run LiveUpdate using: 
    //    
    // do runner.EnableLiveUpdate ()
#endif


#if SAVE_MODEL_WITH_JSON
    let modelId = "model"
    override __.OnSleep() = 

        let json = Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
        Debug.WriteLine("OnSleep: saving model into app.Properties, json = {0}", json)

        app.Properties.[modelId] <- json

    override __.OnResume() = 
        Debug.WriteLine "OnResume: checking for model in app.Properties"
        try 
            match app.Properties.TryGetValue modelId with
            | true, (:? string as json) -> 

                Debug.WriteLine("OnResume: restoring model from app.Properties, json = {0}", json)
                let model = Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>(json)

                Debug.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
                runner.SetCurrentModel (model, Cmd.none)

            | _ -> ()
        with ex ->
            runner.OnError ("Error while restoring model found in app.Properties", ex)

    override this.OnStart() = 
        Debug.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()

#endif
