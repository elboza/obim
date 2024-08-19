open Ast

let car=function
    |Cons (Null,_) -> None
    |Cons (a,_) -> Some a
    |_ -> None

let cdr=function
    |Cons (_,Null) -> None
    |Cons (_,b) -> Some b
    |_ -> None

(*
let (>>=) f g =
    match f with
    |None -> None
    |Some a -> g a
*)
let (>>=) = Option.bind

let caar a=(car a)>>=car
let cadr a=(car a)>>=cdr
let cdar a=(cdr a)>>=car
let cddr a=(cdr a)>>=cdr

(*let rec lookupS name env = match env with 
| Null -> Either.Left "variable not found"
| Item a -> (match a.name=name with
 | true -> Either.Right a
 |_ -> Left "variable not found")
| Cons(a,b) -> match lookupS name a with
 |Either.Left _ -> lookupS name b
 | x -> x
*)

let rec lookup name env = match env with 
| Null -> Either.Left ("variable not found - " ^ name)
| Item a -> (match a.name=name with
| true -> (match a.xval with
|Value a -> Either.Right a
|_ -> Left ("found namespace - " ^ name))
|_ -> Left ("variable not found - " ^ name))
| Cons(a,b) -> match lookup name a with
|Either.Left _ -> lookup name b
| x -> x

let set name xval env =
let item=lookup name env in
match item with
| Right a -> env (*TODO*)
| Left a -> env  (*TODO*)

(* define "s" (Value (Word pippo)) env *)
(* define "s" (Value (Word "pippo")) the_env |> define "s" (Value (Word "pluto"));; *)
let define name xval env = 
let node = (match xval with
| Value a -> Item {name=name;xval= Value a}
| Namespace b -> Item {name=name;xval=Namespace b}) in
Cons(node,env)

let showVal xval=match xval with
| Namespace x -> "Namespace " ^ x
| Value a -> "Value " ^ Ast.show a

let rec show env = match env with
| Null -> "Null"
| Item a -> Printf.sprintf "{name: %s;env_type: %s}" a.name (showVal a.xval)
| Cons(a,b) -> Printf.sprintf "Cons(%s,%s)" (show a) (show b)

let empty_env = Cons(Null,Null)

let the_env = empty_env

let add_env_func func_name func_ref env =
    define func_name (Value (Func func_ref)) env

let takeParamNum p=
    match p with
    | Ast.Num a -> Some a
    | _ -> None

let checkVal varName expVal env=
    let var=lookup varName env in
    match var with
    | Either.Right a -> a=expVal
    | _ -> false

let add_frame params vals env =
    let zippedList= List.combine params vals in
    let add_param p v e = (match p with 
    | Ast.Word pa -> define pa (Value v) e 
    | _ -> env
    ) in
    List.fold_left (fun e item -> add_param (fst item) (snd item) e) env zippedList
