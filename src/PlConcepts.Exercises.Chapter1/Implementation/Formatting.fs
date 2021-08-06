module PlConcepts.Exercises.Chapter1.Formatting

open PlConcepts.Exercises.Chapter1.Types

/// Binary operator priority.
type private OpPriority = byte

/// Lowest operator priority.
let private lowest: OpPriority = System.Byte.MinValue

/// Highest operator priority.
let private highest: OpPriority = System.Byte.MaxValue

/// Get string representation of 'expr' expression.
let rec private formatImpl (parentPriority: OpPriority) (expr: Expr): string =

    let formatPlus left right =
        let priority = 5uy
        let formatWithPriority = formatImpl priority
        let baseFormat = $"{formatWithPriority left} + {formatWithPriority right}"
        if parentPriority > priority then $"({baseFormat})" else baseFormat

    let formatMinus left right =
        let priority = 5uy
        let baseFormat  = $"{formatImpl priority left} - {formatImpl (priority + 1uy) right}"
        if parentPriority > priority then $"({baseFormat})" else baseFormat

    let formatLow = formatImpl lowest

    match expr with
    | Constant value -> string value
    | Var      name  -> name
    | Binary (op, left, right) ->
        match op with
        | Plus     -> formatPlus  left right
        | Minus    -> formatMinus left right
        | Multiply -> let formatMul = formatImpl highest in $"{formatMul left} * {formatMul right}"
        | Max      -> $"max({formatLow left}, {formatLow right})"
        | Min      -> $"min({formatLow left}, {formatLow right})"
    | If (cond, ifTrue, ifFalse) ->
        $"{formatLow cond} = 0 ? {formatLow ifTrue} : {formatLow ifFalse}"

/// Get string representation of 'expr' expression.
let format (expr: Expr): string = formatImpl lowest expr