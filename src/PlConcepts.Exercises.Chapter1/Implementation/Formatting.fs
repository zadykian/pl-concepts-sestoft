module PlConcepts.Exercises.Chapter1.Formatting

open PlConcepts.Exercises.Chapter1.Types

/// Binary operator priority.
type private OpPriority = byte

///// Binary operator associativity.
//type private OpAssociativity =
//    | Left
//    | Right
//    | Invariant

//
///// relative priority and associativity.
//type private OpDef = OpPriority * OpAssociativity
//
//let private opDefinitions: Map<BinaryOp, OpDef> =
//    [
//        Plus,     (5uy, Invariant)
//        Minus,    (5uy, Left)
//        Multiply, (6uy, Invariant)
//    ]
//    |> Map.ofList
//
///// Get definition of binary operator 'op'.
//let private def (op: BinaryOp): OpDef = Map.find op opDefinitions

/// Lowest operator priority.
let private lowest: OpPriority = System.Byte.MinValue

/// Highest operator priority.
let private highest: OpPriority = System.Byte.MaxValue

/// Get string representation of 'expr' expression.
let rec private formatImpl (parentPriority: OpPriority) (expr: Expr): string =
    let formatLow = formatImpl lowest

    match expr with
    | Constant value -> string value
    | Var      name  -> name
    | Binary (op, left, right) ->
        match op with
        | Plus     ->
            let priority = 5uy
            let formatPlus = formatImpl priority
            let baseFormat = $"{formatPlus left} + {formatPlus right}"
            if parentPriority > priority then $"({baseFormat})" else baseFormat

        | Minus    ->
            let priority = 5uy
            let baseFormat  = $"{formatImpl priority left} - {formatImpl (priority + 1uy) right}"
            if parentPriority > priority then $"({baseFormat})" else baseFormat
        
        | Multiply -> let formatMul  = formatImpl highest in $"{formatMul left} * {formatMul right}"
        | Max      -> $"max({formatLow left}, {formatLow right})"
        | Min      -> $"min({formatLow left}, {formatLow right})"
    | If (cond, ifTrue, ifFalse) ->
        $"{formatLow cond} = 0 ? {formatLow ifTrue} : {formatLow ifFalse}"

/// Get string representation of 'expr' expression.
let format (expr: Expr): string = formatImpl lowest expr