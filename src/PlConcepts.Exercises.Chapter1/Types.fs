module PlConcepts.Exercises.Chapter1.Types

/// Binary operator.
type binaryOp =
    | Plus
    | Multiply
    | Subtract
    | Max
    | Min
    | Equals

/// Abstract arithmetic expression.
type expr =
    | CstI of int
    | Var  of string
    | Prim of binaryOp * expr * expr