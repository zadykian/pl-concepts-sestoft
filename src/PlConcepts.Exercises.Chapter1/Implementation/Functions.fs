module PlConcepts.Exercises.Chapter1.Functions

open FSharpPlus
open PlConcepts.Exercises.Chapter1.Types

let private lookup (env: Env) (varName: VarName): int option =
    monad {
        let! _, value = List.tryFind (fun (name, _) -> name = varName) env
        return value
    }

/// Evaluate expression.
let rec eval (expr: Expr) (env: Env): int option = failwith "not implemented!"