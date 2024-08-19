{
open Parser
exception LexicalError of string
}

let number = ['0'-'9']+
let dotted = number ('.' number)?
let blank = [' ' '\t' '\n']
let letter = ['a'-'z' '_']
let word = letter+ 

rule token = parse
 | "let" { LET }
 | "true" { TRUE }
 | "false" { FALSE }
 | blank { token lexbuf }
 | dotted {NUM (float_of_string (Lexing.lexeme lexbuf))}
 | word {WORD (Lexing.lexeme lexbuf) }
 | '+' { PLUS }
 | '-' { MINUS }
 | '*' { MULTIPLY }
 | '/' { DIV }
 | '(' { LPAREN }
 | ')' { RPAREN }
 | '[' { LSPAREN }
 | ']' { RSPAREN }
 | '{' { LSQPAREN }
 | '}' { RSQPAREN }
 | ',' { COMMA }
 | '=' { EQUALS }
 | '!' { NOT }
 | "==" { ISEQUAL }
 | "!=" { NOTEQUAL }
 | "<=" { LE }
 | ">=" { GE }
 | "&&" { AND }
 | "||" { OR }
 | '<' { LT }
 | '>' { GT }
 | '\\' { LAMBDA }
 | ';' { SEMICOLON }
 | '.' { DOT }
 | eof { EOF }
 | _ { raise (LexicalError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }
