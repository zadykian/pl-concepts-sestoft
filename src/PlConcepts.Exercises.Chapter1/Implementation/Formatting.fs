module PlConcepts.Exercises.Chapter1.Formatting

open PlConcepts.Exercises.Chapter1.Types

/// Get string representation of 'expr' expression.
let rec format (expr: Expr): string =
    match expr with
    | Constant value -> string value
    | Var      name  -> name
    | Binary (op, left, right) ->
        let leftFormat = format left
        let rightFormat = format right
        match op with
        | Plus     -> $"{leftFormat} + {rightFormat}"
        | Minus    -> $"{leftFormat} - {rightFormat}"
        | Multiply -> $"{leftFormat} * {rightFormat}"
        | Max      -> $"max({leftFormat}, {rightFormat})"
        | Min      -> $"min({leftFormat}, {rightFormat})"
    | If (cond, ifTrue, ifFalse) -> $"{format cond} = 0 ? {format ifTrue} : {format ifFalse}"