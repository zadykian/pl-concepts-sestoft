module PlConcepts.Exercises.Chapter1.Tests.FormattingTests

open FsUnit
open NUnit.Framework

open PlConcepts.Exercises.Chapter1.Types
open PlConcepts.Exercises.Chapter1.Formatting

/// Format expression 'expr' and compare result with 'expected'.
let private assertFormat (expr: Expr) (expected: string) =
    format expr |> should equal expected

[<Test>]
let ``format single constant`` () =
    let expression = Constant 8
    assertFormat expression "8"