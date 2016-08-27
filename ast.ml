
type binOp = Add
           | Sub
           | Mul
           | Div

type id = string

type unOp = Minus

type expr = Int of int
          | ID of id
          | BinOp of (binOp * expr * expr)
          | UnOp of (unOp * expr)
          | Lambda of (id * expr)
          | Apply of (expr * expr)

type decl = (id * expr)

type program = decl list


let string_of_op op =
    match op with
        Add -> "+"
      | Sub -> "-"
      | Mul -> "*"
      | Div -> "/"


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

let dump_decl (name, expr) =
    Printf.sprintf "(`decl %s %s)" name (dump_expr expr)


let dump_program prog =
    Printf.sprintf "(`prog \n%s)" (String.concat "\t\n" (List.map dump_decl prog))