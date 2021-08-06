module PlConcepts.Exercises.Chapter1.Tests.EvalTests

open FsUnit
open PlConcepts.Exercises.Chapter1.Types
open PlConcepts.Exercises.Chapter1.Functions

open NUnit.Framework

[<Test>]
let ``evaluate constant`` () =
    let expression = Constant 0
    eval expression empty |> should equal (Some 0)

[<Test>]
let ``evaluate sum of two constants`` () =
    let expression = Binary (BinaryOp.Plus, Constant 16, Constant 16)
    eval expression empty |> should equal (Some 32)