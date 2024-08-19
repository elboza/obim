open Printf

type expr =
| Num of float
| Word of string
| Str of string
| Bool of bool
| Apply of expr * expr list
| Quote of expr
| Terror of string
| Func of (expr list -> env -> expr*env)
| Lambda of expr list * expr
| Progn of expr list
| Nop
and env_type=
    | Namespace of string
    | Value of expr
and env=Item of {mutable name:string;mutable xval:env_type} | Cons of env*env|Null


let rec to_string : expr -> string
=fun e ->
match e with
| Num n -> sprintf "Num %G" n
| Word w -> "Word " ^ w
| Str s -> "Str " ^ s
| Nop -> "Nop"
| Bool b -> "Bool " ^ (match b with |true -> "true" | false -> "false")
| Terror s -> "Terror " ^ s
| Quote a -> sprintf "Quote %s" (to_string a)
| Func _ -> sprintf "#func"
| Lambda _ -> sprintf "#lambda"
| Progn [] -> sprintf "Progn ()" 
| Progn (h::t) -> sprintf "Progn (%s [%s])" (to_string h) (List.map to_string t |> String.concat " ")
| Apply(a,b) -> sprintf "(Apply %s [%s])" (to_string a) (List.map to_string b |> String.concat ",")

let rec show : expr -> string
=fun e ->
match e with
| Num n -> sprintf "%G" n
| Word w -> w
| Str s -> s
| Nop -> ""
| Bool b -> (match b with |true -> "true" | false -> "false")
| Terror s -> "Error " ^ s
| Quote a -> show a
| Func _ -> sprintf "#func"
| Lambda _ -> sprintf "#lambda"
| Progn [] -> sprintf "Progn ()" 
| Progn (h::t) -> sprintf "Progn (%s [%s])" (show h) (List.map show t |> String.concat " ")
| Apply(a,b) -> sprintf "(Apply %s [%s])" (show a) (List.map show b |> String.concat ",")

