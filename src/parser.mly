%{

let rec makeLambda args body =
    match args with
        [] -> body
        | h::t -> Ast.Lambda (h, makeLambda t body)

let rec makeApply (func : Ast.expr) (args : Ast.expr list) : Ast.expr =
    match args with
        [] -> func
        | h::t -> makeApply (Ast.Apply (func, h)) t


%}

/* File parser.mly */
%token <int> INT
%token <string> ID
%token VAL
%token EQUALS
%token ROCKET
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token COMMA
%token EOF
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left ROCKET
%nonassoc UMINUS        /* highest precedence */
%start main             /* the entry point */
%type <Ast.program> main
%%

main:
    list(decl) EOF                { $1 }
;

decl:
    VAL ID EQUALS expr { ($2, $4) }
    | VAL ID arglist EQUALS expr { ($2, makeLambda $3 $5) }
;

arglist:
    LPAREN args = separated_list(COMMA, ID) RPAREN  { args }
;

explist:
    LPAREN args = separated_list(COMMA, expr) RPAREN  { args }
;

expr:
    INT                              { Ast.Int $1 }
    | expr PLUS expr                 { Ast.BinOp (Ast.Add, $1, $3) }
    | expr MINUS expr                { Ast.BinOp (Ast.Sub, $1, $3) }
    | expr TIMES expr                { Ast.BinOp (Ast.Mul, $1, $3) }
    | expr DIV expr                  { Ast.BinOp (Ast.Div, $1, $3) }
    | MINUS expr %prec UMINUS        { Ast.UnOp (Ast.Minus, $2) }
    | ID ROCKET expr                 { Ast.Lambda ($1, $3) }
    | base_expr LPAREN expr RPAREN   { Ast.Apply ($1, $3) }
    | base_expr explist              { makeApply $1 $2 }
    | base_expr                      { $1 }
;

base_expr:
    ID                        { Ast.ID $1 }
    | LPAREN expr RPAREN      { $2 }
;

