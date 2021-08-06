module PlConcepts.Exercises.Chapter1.Tests.EvalTests

open FsUnit
open PlConcepts.Exercises.Chapter1.Types
open PlConcepts.Exercises.Chapter1.Functions

open NUnit.Framework

/// Evaluate expression with environment 'env' and compare result with 'expected'.
let private evalAndCheck (env: Env) (expr: Expr) (expected: int)  =
    eval expr env |> should equal (Some expected)

/// Evaluate expression with empty environment and compare result with 'expected'.
let private evalEmptyAndCheck = evalAndCheck empty

[<Test>]
let ``single constant`` () =
    let expression = Constant 0
    evalEmptyAndCheck expression 0

[<Test>]
let ``sum of two constants`` () =
    let expression = Binary (Plus, Constant 16, Constant 16)
    evalEmptyAndCheck expression 32

[<Test>]
let ``difference of two constants`` () =
    let expression = Binary(Minus, Constant 32, Constant 8)
    evalEmptyAndCheck expression 24

[<Test>]
let ``product of two constants`` () =
    let expression = Binary (Multiply, Constant 16, Constant 16)
    evalEmptyAndCheck expression 256

[<Test>]
let ``max of two constants`` () =
    let expression = Binary (BinaryOp.Max, Constant 32, Constant 16)
    evalEmptyAndCheck expression 32

[<Test>]
let ``min of two constants`` () =
    let expression = Binary (Min, Constant 32, Constant 16)
    evalEmptyAndCheck expression 16

[<Test>]
let ``sum of constant and variable`` () =
    let expression  = Binary (Plus, Var "x", Constant 16)
    let environment = ["x", 8] 
    evalAndCheck environment expression 24

[<Test>]
let ``fail if variable is not in scope`` () =
    let expression  = Binary (Plus, Var "x", Constant 16)
    let environment = ["y", 4]
    eval expression environment |> should equal None