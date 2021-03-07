﻿namespace Fabulous.XamarinForms

 open Fabulous
 open Fabulous.LiveUpdate
 open Xamarin.Forms
//
// module LiveUpdate =
//     /// Trace all the updates to the console
//     type Runner<obj, obj, obj, obj> with
//
//         member runner.EnableLiveUpdate() =
//             async {
//                 let syncChangeProgram (changeProgram: unit -> unit) =
//                     Device.BeginInvokeOnMainThread (fun() ->
//                        changeProgram()
//                     )
//
//                 let printAddresses iips httpPort =
//                    if Device.RuntimePlatform = Device.iOS then
//                        printfn "  LiveUpdate: Connect using:"
//                        for iip in iips do
//                            printfn "      dotnet fabulous --watch --webhook:http://%s:%d/update" iip.Address httpPort
//                    elif Device.RuntimePlatform = Device.Android then
//                        printfn "  LiveUpdate: On USB connect using:"
//                        printfn "      adb -d forward  tcp:%d tcp:%d" httpPort httpPort
//                        if httpPort = Ports.DefaultPort then
//                            printfn "      dotnet fabulous --watch --send"
//                        else
//                            printfn "      dotnet fabulous --watch --webhook:http://localhost:%d/update" httpPort
//                            printfn "  "
//                            printfn "  LiveUpdate: On Emulator connect using:"
//                            printfn "      adb -e forward  tcp:%d tcp:%d" httpPort httpPort
//                        if httpPort = Ports.DefaultPort then
//                            printfn "      dotnet fabulous --watch --send"
//                        else
//                            printfn "      dotnet fabulous --watch --webhook:http://localhost:%d/update" httpPort
//                    else
//                        printfn "  LiveUpdate: %s is not officially supported" Device.RuntimePlatform
//                        printfn "  LiveUpdate: You can still try to connect using:"
//                        if httpPort = Ports.DefaultPort then
//                            printfn "      dotnet fabulous --watch --send"
//                        else
//                            printfn "      dotnet fabulous --watch --webhook:http://localhost:%d/update" httpPort
//
//                 Extensions.enableLiveUpdate printAddresses syncChangeProgram runner
//
//             } |> Async.Start
