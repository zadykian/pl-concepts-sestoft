module PlConcepts.Exercises.Chapter1.Tests.FormattingTests

open FsUnit
open NUnit.Framework

open PlConcepts.Exercises.Chapter1.Types
open PlConcepts.Exercises.Chapter1.Formatting

/// Format expression 'expr' and compare result with 'expected'.
let private assertFormat (expr: Expr) (expected: string) =
    format expr |> should equal expected

[<Test>]
let ``single constant`` () =
    let expression = Constant 8
    assertFormat expression "8"

[<Test>]
let ``sum of two constants`` () =
    let expression = Binary (Plus, Constant 16, Constant 32)
    assertFormat expression "16 + 32"

[<Test>]
let ``difference of two constants`` () =
    let expression = Binary(Minus, Constant 32, Constant 8)
    assertFormat expression "32 - 8"

[<Test>]
let ``product of two constants`` () =
    let expression = Binary (Multiply, Constant 16, Constant 32)
    assertFormat expression "16 * 32"

[<Test>]
let ``max of two constants`` () =
    let expression = Binary (BinaryOp.Max, Constant 32, Constant 16)
    assertFormat expression "max(32, 16)"

[<Test>]
let ``min of two constants`` () =
    let expression = Binary (Min, Constant 32, Constant 16)
    assertFormat expression "min(32, 16)"

[<Test>]
let ``sum of constant and variable`` () =
    let expression  = Binary (Plus, Var "x", Constant 16)
    assertFormat expression "x + 16"

[<Test>]
let ``ternary operator left branch`` () =
    let expression  = If (Var "y", Constant 2, Constant 8)
    assertFormat expression "y = 0 ? 2 : 8"

[<Test>]
let ``parentheses in complex expression`` () =
    let expression =
        Binary (
            Multiply,
            Binary (Plus, Var "a", Constant 32),
            Constant 16)

    assertFormat expression "(a + 32) * 16"

[<Test>]
let ``absence of parentheses in expression with equal priority operators`` () =
    let expression =
        Binary (
            Plus,
            Binary (Plus, Var "a", Constant 32),
            Constant 16)

    assertFormat expression "a + 32 + 16"

[<Test>]
let ``parentheses save associativity`` () =
    let expression =
        Binary (
            Minus,
            Constant 16,
            Binary (Plus, Var "a", Constant 32))

    assertFormat expression "16 - (a + 32)"
