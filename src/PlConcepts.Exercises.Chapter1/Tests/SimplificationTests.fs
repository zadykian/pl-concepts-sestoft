module PlConcepts.Exercises.Chapter1.Tests.SimplificationTests

open FsUnit
open NUnit.Framework

open PlConcepts.Exercises.Chapter1.Types
open PlConcepts.Exercises.Chapter1.Simplification

type TestCase = Expr * Expr

/// Simplify expression 'expr' and compare result with 'expected'.
let private assertSimplification (input: Expr) (expected: Expr) =
    simplify input |> should equal expected

let private zero = Constant 0
let private one  = Constant 1

let private testCases =
    seq {
        Constant 32, Constant 32
        Binary (Plus, zero, Var "x"), Var "x"
    }
    |> Seq.map (fun (i, e) -> [|i; e|])
    |> Seq.toArray

[<Test>]
[<TestCaseSource(nameof testCases)>]
let ``run all test cases`` (testCase: TestCase) =
    let input, expected = testCase
    assertSimplification input expected