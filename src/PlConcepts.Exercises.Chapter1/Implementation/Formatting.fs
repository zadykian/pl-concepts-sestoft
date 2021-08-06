module PlConcepts.Exercises.Chapter1.Formatting

open PlConcepts.Exercises.Chapter1.Types

/// Binary operator priority.
type private OpPriority = byte

/// Binary operator associativity.
type private OpAssociativity =
    | Left
    | Right
    | Invariant

/// Definition of operator's behaviour:
/// relative priority and associativity.
type private OpDef = OpPriority * OpAssociativity

let private opDefinitions: Map<BinaryOp, OpDef> =
    [
        Plus,     (5uy, Invariant)
        Minus,    (5uy, Left)
        Multiply, (6uy, Invariant)
    ]
    |> Map.ofList

/// Get definition of binary operator 'op'.
let private def (op: BinaryOp): OpDef = Map.find op opDefinitions 

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