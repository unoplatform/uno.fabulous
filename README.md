UnoFabulous
=======

This is an experiment to get Fabulous working on the Uno Platform.

Must be built using `msbuild`CLI for now under macOS, or VS 2019 for windows.

To get it running (for Wasm only for now):
- Build and run Fabulous.Uno/tools/Fabulous.Uno.Generator/Fabulous.Uno.Generator.fsproj
- Build Fabulous.Uno/samples/CounterApp/CounterApp.fsproj
- Service the application using `python3 server.py` in the generated dist folder