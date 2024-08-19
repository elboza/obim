open Ast
open Env
exception LexicalError of string
exception ParsingError of string

let rec eval : expr -> env -> (expr * env)
=fun e env ->
match e with
| Num n -> (Num n, env)
| Terror s -> (Terror s,env)
| Bool b -> (Bool b, env)
| Word w -> let item=Env.lookup w env in
(match item with
| Either.Right a -> eval a env
| Either.Left b -> Printf.printf "%s\n" b;(Nop, env)
)
| Str s -> (Str s, env)
| Quote a -> (a,env)
| Nop -> (Nop, env)
| Progn [] -> (eval Ast.Nop env)
| Progn (h::t) -> (eval h env) |> snd |> eval (List.hd t)
| Apply(a,b) -> (*(Word (Printf.sprintf "(Apply %s [%s])" (Ast.show a) (List.map (fun b -> eval b env |> fst) b |> List.map Ast.show |> String.concat ",")), env);*)
        (match (eval a env |> fst,env) |> fst with
        | Func f -> (
            let params=List.map(fun b-> eval b env |> fst) b in
            (f params env)
        )
        | Lambda(params,body) -> (*(Terror (Printf.sprintf "%s #lambda apply" (Ast.show a)), env)*)
                (
                    let env_frame=Env.add_frame params (List.map (fun b-> eval b env |> fst) b) env in
                    (eval body env_frame |> fst, env)
                )
        | _ -> (Terror (Printf.sprintf "%s is not a function" (Ast.show a)), env)
        )
| Func f -> (Func f, env) (*(f [Ast.Num 2.;Ast.Num 3.] env)*) (*(Quote (Word (Printf.sprintf "#func...")), env)*)
| Lambda(f,b) -> (Lambda(f,b),env) (*(Quote (Word (Printf.sprintf "#lambda...")),env)*)

let print_error_position lexbuf = 
    let pos = lexbuf.Lexing.lex_curr_p in
    Printf.sprintf "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let init_env=
    let empty_env=Cons(Null,Null) in
    empty_env |> Native_lib.populate_env

let exec lexbuf the_env = 
 	let ast = (try Ok(Parser.program Lexer.token lexbuf) with
    | LexicalError msg -> 
            let error_msg = Printf.sprintf "%s: %s@." (print_error_position lexbuf) msg in
            Error (error_msg)
    | _ ->
            let error_msg = Printf.sprintf "%s: syntax error@." (print_error_position lexbuf) in
            Error (error_msg)
    ) in
    match ast with
    |Ok ast -> (
        if((Env.checkVal "__debug" (Ast.Num 1.) the_env)=true) then print_endline (Ast.to_string ast);
 		if((Env.checkVal "__debug" (Ast.Num 1.) the_env)=true) then print_endline (Ast.show ast);
        let resp=eval ast the_env in
        print_endline (resp |> fst |> if((Env.checkVal "__debug" (Ast.Num 1.) the_env)=true) then Ast.to_string else Ast.show);
        if((Env.checkVal "__debug" (Ast.Num 1.) the_env)=true) then print_endline (resp |> snd |> Env.show);
 		resp |> snd
    ) 
    | Error msg -> (*raise (ParsingError msg)*) print_endline msg; the_env


(*
func Word-> [exp]-> env-> (exp*env)
lambda [word]-> [exp]-> [exp]->env-> (exp*env)
apply word->[exp]->env->(exp*env)

env=add_env("pippo",pippo,env)

let pippo=fun [exp] env->
    let val= takeParam Ast.word [exp] in
    match val with
    |Right a -> Printf.printf "%s" (a)
    |Left b -> showErrorMsg b


Quote .....
*)
