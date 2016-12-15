
module IdMap = Map.Make(struct type t = Ast.id let compare = compare end)


open Llvm_executionengine
exception UndefinedID of string

type llvaluemap = (Ast.id, (Llvm.lltype * Llvm.llvalue)) Hashtbl.t
type ctx = 
    { values : llvaluemap ;
      builder : Llvm.llbuilder ;
      llmod : Llvm.llmodule ;
      llctx : Llvm.llcontext ;
    }

let llctx = Llvm.global_context ()
let int_type = Llvm.i64_type llctx


let bool_to_int (builder : Llvm.llbuilder) (x : Llvm.llvalue) : Llvm.llvalue =
    Llvm.build_select x (Llvm.const_int int_type 1) (Llvm.const_int int_type 0) "" builder


let rec gen_expr (ctx : ctx) (name : string) (expr : Ast.expr) : (Llvm.lltype * Llvm.llvalue) =
    match expr with
    Ast.Int i -> (int_type, Llvm.const_int int_type i)
    | Ast.ID id -> (try Hashtbl.find ctx.values id with
        | Not_found -> raise (UndefinedID id))
    | Ast.BinOp (op, a, b) ->
     (let (_, lhs) = gen_expr ctx "" a in
      let (_, rhs) = gen_expr ctx "" b in
      (int_type, (match op with
            Ast.Add -> Llvm.build_add lhs rhs name ctx.builder
            | Ast.Mul -> Llvm.build_mul lhs rhs name ctx.builder
            | Ast.Div -> Llvm.build_sdiv lhs rhs name ctx.builder
            | Ast.Sub -> Llvm.build_sub lhs rhs name ctx.builder

            | Ast.Eq -> Llvm.build_icmp Llvm.Icmp.Eq lhs rhs name ctx.builder |> bool_to_int ctx.builder 
            | Ast.Lt -> Llvm.build_icmp Llvm.Icmp.Slt lhs rhs name ctx.builder |> bool_to_int ctx.builder 
            | Ast.Gt -> Llvm.build_icmp Llvm.Icmp.Sgt lhs rhs name ctx.builder |> bool_to_int ctx.builder 
            | Ast.Lte -> Llvm.build_icmp Llvm.Icmp.Sle lhs rhs name ctx.builder |> bool_to_int ctx.builder 
            | Ast.Gte -> Llvm.build_icmp Llvm.Icmp.Sge lhs rhs name ctx.builder |> bool_to_int ctx.builder 
     )))
    | Ast.Lambda (arg, body) ->
        let ftype = Llvm.function_type int_type [| int_type |] in
        let fn = Llvm.declare_function name ftype ctx.llmod in

        let _ = Hashtbl.add ctx.values name (ftype, fn) in

        let param = Llvm.param fn 0 in
        let _ = Llvm.set_value_name arg param in
        let _ = Hashtbl.add ctx.values arg (int_type, param) in

        let bb = Llvm.append_block ctx.llctx "entry" fn in
        let _ = Llvm.position_at_end bb ctx.builder in
        let (ret_type, body_value) = gen_expr ctx "" body in
        Llvm.build_ret body_value ctx.builder;
        (ftype, fn)
    | Ast.Pos (exp, pos) -> gen_expr ctx name exp
    | Ast.Apply (fn, arg) ->
        let (_, fn_val) = gen_expr ctx "fn" fn in
        let (_, arg_val) = gen_expr ctx "arg" arg in
        (int_type, Llvm.build_call fn_val [|arg_val|] "" ctx.builder)
    | Ast.If (cond, conseq, alt) ->
        let (_, cond_val) = gen_expr ctx "" cond in
        let start_block = Llvm.insertion_block ctx.builder in
        let parent_func = Llvm.block_parent start_block in

        let conseq_block = Llvm.append_block ctx.llctx "conseq" parent_func in
        let _ = Llvm.position_at_end conseq_block ctx.builder in
        let (conseq_type, conseq_val) = gen_expr ctx "" conseq in
        let new_conseq_block = Llvm.insertion_block ctx.builder in

        let alt_block = Llvm.append_block ctx.llctx "alt" parent_func in
        let _ = Llvm.position_at_end alt_block ctx.builder in
        let (alt_type, alt_val) = gen_expr ctx "" alt in
        let new_alt_block = Llvm.insertion_block ctx.builder in

        let merge_block = Llvm.append_block ctx.llctx "ifmerge" parent_func in
        let _ = Llvm.position_at_end merge_block ctx.builder in
        let incoming = [(conseq_val, new_conseq_block); (alt_val, new_alt_block)] in
        let phi = Llvm.build_phi incoming "iftmp" ctx.builder in

        let _ = Llvm.position_at_end start_block ctx.builder in
        let _ = Llvm.build_cond_br cond_val conseq_block alt_block ctx.builder in

        let _ = Llvm.position_at_end new_conseq_block ctx.builder in
        let _ = Llvm.build_br merge_block ctx.builder in

        let _ = Llvm.position_at_end new_alt_block ctx.builder in
        let _ = Llvm.build_br merge_block ctx.builder in

        let _ = Llvm.position_at_end merge_block ctx.builder in
        (conseq_type, phi)


let gen_decl (ctx : ctx) ((name, value) : Ast.decl) : unit =
    let (typ, value) = gen_expr ctx name value in
    let _ = Hashtbl.add ctx.values name (typ, value) in
    ()


let gen_module (prog : Ast.program) : Llvm.llmodule =
    let llmod = Llvm.create_module llctx "fb" in
    let builder = Llvm.builder llctx in
    let ctx = { values = Hashtbl.create 10 ; builder = builder ; llmod = llmod ; llctx = llctx } in
    List.iter (gen_decl ctx) prog;
    llmod


let interp (prog : Ast.program) : int =
    let llmod = gen_module prog in
    let main_opt = Llvm.lookup_function "main" llmod in
    let execution_engine = ExecutionEngine.create llmod in
    match main_opt with
    None -> raise (UndefinedID "add a main function")
    | Some f ->
        let output = ExecutionEngine.run_function f [| GenericValue.of_int int_type 0 |] execution_engine in
        GenericValue.as_int output

