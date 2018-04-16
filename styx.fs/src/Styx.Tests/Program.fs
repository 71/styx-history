module Program

open System.Collections.Generic
open System.Reflection

open NUnit.Framework.Api
open NUnit.Framework.Internal

/// Entry point of the program, used when debugging tests through the
/// .NET Core debugger.
[<EntryPoint>]
let main _ =
    let builder = DefaultTestAssemblyBuilder()
    let runner = NUnitTestAssemblyRunner(builder)

    runner.Load(Assembly.GetExecutingAssembly(), Dictionary()) |> ignore

    let result = runner.Run(TestListener.NULL, TestFilter.Empty)

    if result.InconclusiveCount = 0 then 0 else 1
