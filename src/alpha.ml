
open Dict


type idDict = (Ast.id, Ast.id) dict

let print_idDict (d : idDict) =
    print_string (dump (fun x -> x) (fun x -> x) d);
    print_newline()


exception UndefinedID of string

let rec convert prog =
        convert_inner Empty prog

    and convert_inner env prog =
        match prog with
            h::t -> 
                let (newEnv, newDecl) = convert_decl env h in
                newDecl::(convert_inner newEnv t)
          | [] -> []
    and convert_decl env (name, value) =
        let (newEnv, newName) = alloc_name env name name in
        let (newEnv2, newValue) = convert_expr newEnv value in
        (newEnv2, (newName, newValue))

    and convert_expr env expr =
        match expr with
            Ast.Int i -> (env, Ast.Int i)
          | Ast.ID id -> 
            (let newName = lookup env id in
            match newName with
                None -> raise (UndefinedID id)
              | Some name -> (env, Ast.ID name))
          | Ast.BinOp (op, a, b) -> 
            let (newEnv, lhs) = convert_expr env a in
            let (newEnv2, rhs) = convert_expr newEnv b in
            (newEnv2, Ast.BinOp (op, lhs, rhs))
          | Ast.UnOp (op, a) -> 
            let (newEnv, lhs) = convert_expr env a in
            (newEnv, Ast.UnOp (op, lhs))
          | Ast.Apply (func, arg) -> 
            let (newEnv, lhs) = convert_expr env func in
            let (newEnv2, rhs) = convert_expr newEnv arg in
            (newEnv2, Ast.Apply (lhs, rhs))
          | Ast.Lambda (arg, body) ->
            let (newEnv, newArg) = alloc_name env arg arg in
            let (newEnv2, newBody) = convert_expr newEnv body in
            (newEnv2, Ast.Lambda (newArg, newBody))

    and alloc_name (env : idDict) (orig : Ast.id) (name : Ast.id) : (idDict * Ast.id) =
        let (current : Ast.id option) = lookup env name in
        match current with
            None -> (append env orig name, name)
          | Some other -> alloc_name env orig (other ^ "'")
