
type binOp = Add
           | Sub
           | Mul
           | Div
           | Eq
           | Lt
           | Gt
           | Lte
           | Gte


type id = string

type unOp = Minus

type expr = Int of int
          | ID of id
          | BinOp of (binOp * expr * expr)
          | UnOp of (unOp * expr)
          | Lambda of (id * expr)
          | Apply of (expr * expr)
          | If of (expr * expr * expr)
          | Pos of (expr * Lexing.position)

type decl = (id * expr)

type program = decl list


let string_of_op op =
    match op with
        Add -> "+"
      | Sub -> "-"
      | Mul -> "*"
      | Div -> "/"
      | Eq -> "=="
      | Lt -> "<"
      | Gt -> ">"
      | Lte -> "<="
      | Gte -> ">="


let string_of_unop op =
    match op with
        Minus -> "-"

 
let rec dump_expr expr =
    match expr with
        Int i -> string_of_int i
      | ID id -> id
      | BinOp (op, lhs, rhs) -> Printf.sprintf "(`binop %s %s %s)" (string_of_op op) (dump_expr lhs) (dump_expr rhs)
      | UnOp (op, inner) -> Printf.sprintf "(`unop %s %s)" (string_of_unop op) (dump_expr inner)
      | Lambda (arg, body) -> Printf.sprintf "(`lambda %s %s)" arg (dump_expr body)
      | Apply (func, arg) -> Printf.sprintf "(`call %s %s)" (dump_expr func) (dump_expr arg)
      | If (cond, conseq, alt) -> Printf.sprintf "(`if %s %s %s)" (dump_expr cond) (dump_expr conseq) (dump_expr alt)

let dump_decl (name, expr) =
    Printf.sprintf "(`decl %s %s)" name (dump_expr expr)


let dump_program prog =
    Printf.sprintf "(`prog %s)" (String.concat ";" (List.map dump_decl prog))