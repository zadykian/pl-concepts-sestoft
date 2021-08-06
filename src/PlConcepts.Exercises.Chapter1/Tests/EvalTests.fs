module PlConcepts.Exercises.Chapter1.Tests.EvalTests

open FsUnit
open PlConcepts.Exercises.Chapter1.Types
open PlConcepts.Exercises.Chapter1.Functions

open NUnit.Framework

let private evalAndCheck (expr: Expr) (expected: int) =
    eval expr empty |> should equal (Some expected)

[<Test>]
let ``evaluate constant`` () =
    let expression = Constant 0
    evalAndCheck expression 0

[<Test>]
let ``evaluate sum of two constants`` () =
    let expression = Binary(Plus, Constant 16, Constant 16)
    evalAndCheck expression 32

[<Test>]
let ``evaluate difference of two constants`` () =
    let expression = Binary(Minus, Constant 32, Constant 8)
    evalAndCheck expression 24

[<Test>]
let ``evaluate product of two constants`` () =
    let expression = Binary(Multiply, Constant 16, Constant 16)
    evalAndCheck expression 256

[<Test>]
let ``max of two constants`` () =
    let expression = Binary(BinaryOp.Max, Constant 32, Constant 16)
    evalAndCheck expression 32

[<Test>]
let ``min of two constants`` () =
    let expression = Binary(Min, Constant 32, Constant 16)
    evalAndCheck expression 16