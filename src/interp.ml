module IdMap = Map.Make(struct type t = Ast.id let compare = compare end)


open Llvm_executionengine

exception NotCallable of string
exception TypeError of string
exception UndefinedID of Lexing.position * string

type valuedict = (value ref) IdMap.t

and ctx = 
    { values : valuedict;
      position : Lexing.position;
    }
and value =
    Int of int
  | Lambda of (Ast.id * Ast.expr * ctx)
  | True
  | False


let empty_ctx = {values = IdMap.empty; 
                 position = {
                    pos_lnum = 0; 
                    pos_fname = "todo.fb"; 
                    pos_bol = 0; 
                    pos_cnum = 0; 
                }}



let rec string_of_value v =
    match v with
        Int i -> Printf.sprintf "%d" i
      | Lambda (id, exp, ctx) -> Printf.sprintf "(lambda %s (%s) -> {%s})"
            (*(dump (fun x->x) string_of_value ctx.values)*) "TODO: add ctx"
            id (Ast.dump_expr exp)
      | True -> "true"
      | False -> "false"

let assert_int x =
    match x with
    Int i -> Int i
    | _ -> raise (TypeError (Printf.sprintf "expected type int, found %s" (string_of_value x)))

let rec eval ctx exp =
    match exp with
    Ast.Pos (expr, position) ->
      eval {ctx with position = position} expr

    | Ast.Int i -> Int i

    | Ast.ID id ->
    (try
        !(IdMap.find id ctx.values)
    with
    | Not_found -> raise (UndefinedID (ctx.position, id)))
    
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
    
    | Ast.If (cond, conseq, alt) -> 
    let condVal = eval ctx cond in
    match condVal with
        True -> eval ctx conseq
      | _ -> eval ctx alt

and call (func : value) (arg : value) =
    match func with
        Lambda (param, body, ctx) ->
            let newEnv = IdMap.add param (ref arg) ctx.values in
            eval {values=newEnv; position=ctx.position} body
      | _ -> raise (NotCallable (string_of_value func))

let update_binding (values : valuedict) (name : string) (value : value) : unit =
    let f k v =
        if k == name then
            v := value
        else 
            ()
    in
    IdMap.iter f values 

let prepare_decl (env : ctx) ((name, value) : Ast.decl) : ctx =
    let (values : valuedict) = IdMap.add name (ref (Int 0)) env.values in
    let newEnv = { values = values; position = env.position } in
    let _ = update_binding values name (eval newEnv value) in
    newEnv

let prepare (prog : Ast.program) : ctx =
    List.fold_left prepare_decl empty_ctx prog

let interp (prog : Ast.program) : value =
    let ctx = prepare prog in
    let main = !(IdMap.find "main" ctx.values) in
    call main (Int 0) 

