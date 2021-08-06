module PlConcepts.Exercises.Chapter1.Formatting

open PlConcepts.Exercises.Chapter1.Types

/// Get string representation of 'expr' expression.
let rec format (expr: Expr): string =
    match expr with
    | Constant value -> string value
    | _ -> failwith "not implemented!"