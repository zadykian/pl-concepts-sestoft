module PlConcepts.Exercises.Chapter1.Functions

open FSharpPlus
open PlConcepts.Exercises.Chapter1.Types

let private lookup (env: Env) (varName: VarName): int option =
    monad {
        let! _, value = List.tryFind (fun (name, _) -> name = varName) env
        return value
    }

/// Empty expression evaluation environment.
let empty: Env = []

/// Evaluate expression.
let rec eval (expr: Expr) (env: Env): int option =
    match expr with
    | Constant value -> Some value
    | Binary (op, left, right) ->
        match op with
        | BinaryOp.Plus -> monad {
                let! leftRes = eval left env
                let! rightRes = eval right env
                return leftRes + rightRes
            }
        | _ -> failwith "not implemented!"
    | _ -> failwith "not implemented!"