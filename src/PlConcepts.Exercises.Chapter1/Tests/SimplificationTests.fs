module PlConcepts.Exercises.Chapter1.Tests.SimplificationTests

open FsUnit
open NUnit.Framework

open PlConcepts.Exercises.Chapter1.Types
open PlConcepts.Exercises.Chapter1.Simplification

type TestCase = Expr * Expr

let private zero = Constant 0

let private one  = Constant 1

let private (@+) left right = Binary (Plus, left, right)

let private (@-) left right = Binary (Minus, left, right)

let private (@*) left right = Binary (Multiply, left, right)

let private testCases =
    seq {
        (* input *)                         (* expected *)
        Constant 32                       , Constant 32
        Var "y"                           , Var "y"
        zero @+ Var "x"                   , Var "x"
        Var "a" @- Var "a"                , zero
        Var "a" @- zero                   , Var "a"
        Constant 16 @* one                , Constant 16
        zero @* Var "x"                   , zero
        (one @+ zero) @* (Var "x" @+ zero), Var "x"
    }
    |> Seq.map (fun (i, e) -> [|i; e|])
    |> Seq.toArray

[<Test>]
[<TestCaseSource(nameof testCases)>]
let ``run all test cases`` (testCase: TestCase) =
    let input, expected = testCase
    simplify input |> should equal expected