module PlConcepts.Exercises.Chapter1.Types

/// Binary operator.
type binaryOp =
    | Plus
    | Multiply
    | Subtract
    | Max
    | Min
    | Equals

/// Variable name.
type VarName = string

/// Abstract arithmetic expression.
type expr =
    | Constant of int
    | Var      of VarName
    | Binary   of binaryOp * expr * expr

/// Evaluation environment which contains named variables.
type env = (VarName * int) list    