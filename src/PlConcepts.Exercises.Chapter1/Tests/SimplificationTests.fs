module PlConcepts.Exercises.Chapter1.Tests.SimplificationTests

open FsUnit
open NUnit.Framework

open PlConcepts.Exercises.Chapter1.Types
open PlConcepts.Exercises.Chapter1.Simplification

/// Simplify expression 'expr' and compare result with 'expected'.
let private assertSimplification (input: Expr) (expected: Expr) =
    simplify input |> should equal expected

[<Test>]
let ``constant can't be more simple`` () =
    let expression = Constant 32
    assertSimplification expression (Constant 32)

