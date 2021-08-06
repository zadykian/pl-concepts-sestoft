module PlConcepts.Exercises.Chapter1.Types

/// Binary operator.
type BinaryOp =
    | Plus
    | Multiply
    | Minus
    | Max
    | Min

/// Variable name.
type VarName = string

/// Abstract arithmetic expression.
type Expr =
    | Constant of int
    | Var      of VarName
    | Binary   of BinaryOp * Expr * Expr

/// Evaluation environment which contains named variables.
type Env = (VarName * int) list