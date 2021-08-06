module PlConcepts.Exercises.Chapter1.Functions

open FSharpPlus
open PlConcepts.Exercises.Chapter1.Types

let private lookup (env: Env) (varName: VarName) : int option =
    monad {
        let! _, value = List.tryFind (fun (name, _) -> name = varName) env
        return value
    }

/// Empty expression evaluation environment.
let empty: Env = []

/// Evaluate expression.
let rec eval (expr: Expr) (env: Env) : int option =
    match expr with
    | Constant value -> Some value
    | Binary (op, leftExpr, rightExpr) ->
        monad {
            let! left = eval leftExpr env
            let! right = eval rightExpr env
            return
                match op with
                | Plus     -> left + right
                | Minus    -> left - right
                | Multiply -> left * right
                | Max      -> max left right
                | Min      -> min left right
        }
    | Var name -> lookup env name