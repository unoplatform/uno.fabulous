open System
open Windows.UI.Xaml

[<EntryPoint; STAThread>]
let main argv =

    Application.Start(fun _ -> new CounterApp.CounterApp() |> ignore)
    0