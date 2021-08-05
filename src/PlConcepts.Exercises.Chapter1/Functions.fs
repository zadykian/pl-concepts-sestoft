module PlConcepts.Exercises.Chapter1.Functions

open PlConcepts.Exercises.Chapter1.Types

/// Evaluation environment which contains named variables.
type env = (string * int) list

/// Evaluate expression.
let rec eval (expr: expr) (env: env): int = failwith "not implemented!"