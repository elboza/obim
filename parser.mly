%{
%}

%token NEWLINE LPAREN RPAREN PLUS MINUS MULTIPLY DIV EOF COMMA 
%token LAMBDA DOT LSPAREN RSPAREN SEMICOLON
%token LSQPAREN RSQPAREN TRUE FALSE
%token ISEQUAL NOTEQUAL LE GE AND OR LT GT NOT
%token LET EQUALS POSTOP
%token <float> NUM
%token <string> WORD

%left SEMICOLON
%left AND OR
%left ISEQUAL NOTEQUAL LE GE GT LT
%left NOT
%left PLUS MINUS
%left MULTIPLY DIV
%nonassoc UMINUS

%start program
%type <Ast.expr> program

%%

program : BLOCK_CODE EOF { $1 }

exp:
| LET WORD EQUALS exp { Ast.Apply(Ast.Word("_sys_assign"),[Ast.Quote(Ast.Word($2));$4]) }
| NUM {Ast.Num ($1) }
| TRUE {Ast.Bool(true)}
| FALSE {Ast.Bool(false)}
| LSQPAREN BLOCK_CODE RSQPAREN {$2}
| exp PLUS exp { Ast.Apply(Ast.Word "_sys_add", [$1;$3]) }
| exp MULTIPLY exp { Ast.Apply(Ast.Word "_sys_mul", [$1;$3]) }
| exp MINUS exp { Ast.Apply(Ast.Word "_sys_sub", [$1;$3]) }
| exp DIV exp { Ast.Apply(Ast.Word "_sys_div", [$1;$3]) }
| exp ISEQUAL exp { Ast.Apply(Ast.Word "_sys_isequal", [$1;$3]) }
| exp NOTEQUAL exp { Ast.Apply(Ast.Word "_sys_notequal", [$1;$3]) }
| exp GE exp { Ast.Apply(Ast.Word "_sys_ge", [$1;$3]) }
| exp LE exp { Ast.Apply(Ast.Word "_sys_le", [$1;$3]) }
| exp AND exp { Ast.Apply(Ast.Word "_sys_and", [$1;$3]) }
| exp OR exp { Ast.Apply(Ast.Word "_sys_or", [$1;$3]) }
| NOT exp %prec NOT { Ast.Apply(Ast.Word "_sys_not", [$2]) }
| exp GT exp { Ast.Apply(Ast.Word "_sys_gt", [$1;$3]) }
| exp LT exp { Ast.Apply(Ast.Word "_sys_lt", [$1;$3]) }
| WORD LPAREN FUNC_PARAMS RPAREN { Ast.Apply(Ast.Word($1),$3) }
| LAMBDA_F LSPAREN FUNC_PARAMS RSPAREN { Ast.Apply($1,$3) }
| LAMBDA_F { $1 }
| LPAREN exp RPAREN { $2 }
| WORD { Ast.Word ($1) }
| MINUS exp %prec UMINUS { Ast.Apply(Ast.Word "_sys_mul", [Ast.Num(-1.);$2]) }

BLOCK_CODE:
    | exp {$1}
    | exp SEMICOLON BLOCK_CODE { Ast.Progn($1::[$3]) }
FUNC_PARAMS:
    | {[Ast.Nop]}
    | FUNC_PARAMS_LIST {$1}

FUNC_PARAMS_LIST:
    | exp {[$1]}
    | exp COMMA FUNC_PARAMS_LIST {List.append [$1] $3}

LAMBDA_F:
    | LAMBDA LAMBDA_PARAMS DOT LAMBDA_BODY { Ast.Lambda($2,$4) }

LAMBDA_PARAMS:
    | {[Ast.Nop]}
    | LAMBDA_PARAMS_LIST {$1}

LAMBDA_PARAMS_LIST:
    | WORD {[Ast.Word($1)]}
    | WORD COMMA LAMBDA_PARAMS_LIST {List.append [Ast.Word($1)] $3}

LAMBDA_BODY:
    exp {$1}

