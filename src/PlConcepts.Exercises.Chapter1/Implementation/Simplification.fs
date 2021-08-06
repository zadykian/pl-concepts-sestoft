module PlConcepts.Exercises.Chapter1.Simplification

open PlConcepts.Exercises.Chapter1.Types

let private zero = Constant 0

/// If possible, simplify expression 'expr'.
let rec simplify (expr: Expr): Expr =
    match expr with
    | Binary (op, left, right) ->
        let defaultBinary op = simplify (Binary (op, simplify left, simplify right))

        match op with
        | Plus ->
            match (left, right) with
            | Constant 0, _ -> simplify right
            | _, Constant 0 -> simplify left
            | _ -> defaultBinary Plus

        | Minus ->
            match (left, right) with
            | _, Constant 0 -> simplify left
            | _ when left.Equals right -> zero
            | _ -> defaultBinary Minus

        | Multiply ->
            match (left, right) with
            | Constant 1, _ -> simplify right
            | _, Constant 1 -> simplify left
            | Constant 0, _ -> zero
            | _, Constant 0 -> zero
            | _ -> defaultBinary Multiply

        | Max -> defaultBinary Max
        | Min -> defaultBinary Min
    | _ -> expr