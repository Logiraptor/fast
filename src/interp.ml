open Dict


exception NotCallable of string
exception TypeError of string

type valuedict = (Ast.id, value) dict

and ctx = 
    { values : valuedict;
    }
and value =
    Int of int
  | Lambda of (Ast.id * Ast.expr * ctx)
  | True
  | False


let empty_ctx = {values = Empty}



let rec string_of_value v =
    match v with
        Int i -> Printf.sprintf "(int %d)" i
      | Lambda (id, exp, ctx) -> Printf.sprintf "(lambda (%s) -> %s)" 
            id (Ast.dump_expr exp)
      | True -> "(true)"
      | False -> "(false)"

let assert_int x =
    match x with
    Int i -> Int i
    | _ -> raise (TypeError (Printf.sprintf "expected type int, found %s" (string_of_value x)))

let rec eval ctx exp =
    match exp with
      Ast.Int i -> Int i

    | Ast.ID id -> 
    (let v = lookup ctx.values id in
    match v with
        None -> raise (Alpha.UndefinedID id)
      | Some value -> value)
    
    | Ast.Apply (func, arg) ->
    (let f = eval ctx func in
    let a = eval ctx arg in
    call f a
    )

    | Ast.BinOp (op, a, b) ->
    (let Int lhs = assert_int @@ eval ctx a in
    let Int rhs = assert_int @@ eval ctx b in
    match op with
        Ast.Add -> Int (lhs + rhs)
        | Ast.Mul -> Int (lhs * rhs)
        | Ast.Div -> Int (lhs / rhs)
        | Ast.Sub -> Int (lhs - rhs)
        | Ast.Eq -> if lhs == rhs then True else False
        | Ast.Lt -> if lhs < rhs then True else False
        | Ast.Gt -> if lhs > rhs then True else False
        | Ast.Lte -> if lhs <= rhs then True else False
        | Ast.Gte -> if lhs >= rhs then True else False
    )
    
    | Ast.Lambda (arg, body) -> Lambda (arg, body, ctx)
 
and call (func : value) (arg : value) =
    match func with
        Lambda (param, body, ctx) ->
            let newEnv = append ctx.values param arg in
            eval {values=newEnv} body
      | _ -> raise (NotCallable (string_of_value func))


let prepare_prog (env : ctx) ((name, value) : Ast.decl) : ctx =
    let body = eval env value in
    let (values : valuedict) = append env.values name body in
    { values = values
    }

let prepare (prog : Ast.program) : ctx =
    List.fold_left prepare_prog empty_ctx prog

let interp (prog : Ast.program) : value =
    let ctx = prepare prog in
    let Some main = lookup ctx.values "main" in
    call main (Int 0) 
