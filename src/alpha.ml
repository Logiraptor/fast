

module IdMap = Map.Make(struct type t = Ast.id let compare = compare end)


type renaming = Ast.id IdMap.t

exception UndefinedID of string
exception RedefinedID of string

let rec convert prog =
        let names = alloc_names IdMap.empty prog in
        convert_inner names prog

    and convert_inner env prog =
        match prog with
            h::t -> 
                let (newEnv, newDecl) = convert_decl env h in
                newDecl::(convert_inner newEnv t)
          | [] -> []
    and convert_decl env (name, value) =
        let (newEnv, newValue) = convert_expr env value in
        (newEnv, (lookup_name env name, newValue))

    and convert_expr env expr =
        match expr with
            Ast.Int i -> (env, Ast.Int i)
          | Ast.ID id -> 
            (env, Ast.ID (lookup_name env id))
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
          | Ast.If (cond, conseq, alt) ->
            let (newEnv1, newCond) = convert_expr env cond in
            let (newEnv2, newConseq) = convert_expr newEnv1 conseq in
            let (newEnv3, newAlt) = convert_expr newEnv1 alt in
            (newEnv3, Ast.If (newCond, newConseq, newAlt))
          | Ast.Pos (expr, position) ->
            let (newEnv, newExpr) = convert_expr env expr in
            (newEnv, Ast.Pos (newExpr, position))

    and alloc_names (env : renaming) decl : renaming =
        match decl with
        | [] -> env
        | (id, _)::t ->
        if IdMap.mem id env then
            raise (RedefinedID id)
        else
            let (newEnv, _) = (alloc_name env id id) in
            alloc_names newEnv t 

    and alloc_name (env : renaming) (orig : Ast.id) (name : Ast.id) : (renaming * Ast.id) =
        if IdMap.mem name env then
            alloc_name env orig (IdMap.find name env ^ "'")
        else
            (IdMap.add orig name env, name)


    and lookup_name (env : renaming) (id : Ast.id) : Ast.id =
        if IdMap.mem id env then
            IdMap.find id env
        else
            raise (UndefinedID id)