open Printf
open Ocamline

let rec repl2 env () =
    let str=read ~brackets:['(',')'; '[',']'] ~prompt:">>>" ~history_loc:"~/.ocamline_history" () in
    match str with
    | "bye" -> print_endline "bye"
    | _ -> let r_env=Eval.exec (Lexing.from_string str) env in
     (flush stdout; repl2 r_env ())

let repl () = 
    printf "repl...\n";
    flush stdout;
    repl2 Eval.init_env ()
