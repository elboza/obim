exception ListReduce of string
let pippo=fun _ env -> Printf.printf "pippof...\n";(Ast.Nop, env)
let pluto=fun _ env -> Printf.printf "plutof...\n";(Ast.Nop, env)
let reduce f list =
    match list with
    |head::tail -> List.fold_left f head tail
    |[] -> raise (ListReduce "empty params list")
let binop=fun op args env ->
    let resp=List.map Env.takeParamNum args |> reduce (fun a b ->
    match (a,b) with
    | (Some x,Some y) -> Some ((op) x y)
    | (_,_) -> None
) in
    match resp with
    | Some a -> (Ast.Num a,env)
    | None -> (Ast.Terror "invalid params", env)
let binopb=fun op args env ->
    let params=List.map Env.takeParamNum args in
     match List.for_all (fun a -> match a with |None -> false | Some _ -> true) params with
     | false -> (Ast.Terror "invalid param",env)
     | true -> let resp = (List.map (fun a -> (op) (List.hd params) a) (List.tl params) |> List.exists (fun a-> a=true)) in (Ast.Bool resp, env)
let add=fun args env ->
    binop (+.) args env
let sub=fun args env ->
    binop (-.) args env
let mul=fun args env ->
    binop ( *. ) args env
let div=fun args env ->
    binop (/.) args env
let assign=fun args env ->
    let varName=List.nth_opt args 0 in
    let varValue=List.nth_opt args 1 in
    match (varName,varValue) with
    | (Some Ast.Word a,Some b) -> (b, Env.define a (Ast.Value b) env)
    | (_,_) -> (Ast.Terror "Error assigning value", env)
let _sys_isequal=fun args env ->
    binopb ( = ) args env
let _sys_notequal=fun args env ->
    binopb ( <> ) args env
let _sys_ge=fun args env ->
    binopb ( >= ) args env
let _sys_le=fun args env ->
    binopb ( <= ) args env
let _sys_and=fun args env ->
    binopb ( == ) args env
let _sys_or=fun args env ->
    binopb ( == ) args env
let _sys_not=fun args env ->
    binopb ( == ) args env
let _sys_gt=fun args env ->
    binopb ( > ) args env
let _sys_lt=fun args env ->
    binopb ( < ) args env
let populate_env env=
    env |>
    Env.define "__debug" (Value (Ast.Num 1.)) |>  (* !!!! *)
    Env.add_env_func "pippo" pippo |>
    Env.add_env_func "pluto" pluto |>
    Env.add_env_func "_sys_assign" assign |>
    Env.add_env_func "_sys_isequal" _sys_isequal |>
    Env.add_env_func "_sys_notequal" _sys_notequal |>
    Env.add_env_func "_sys_ge" _sys_ge |>
    Env.add_env_func "_sys_le" _sys_le |>
    Env.add_env_func "_sys_and" _sys_and |>
    Env.add_env_func "_sys_or" _sys_or |>
    Env.add_env_func "_sys_not" _sys_not |>
    Env.add_env_func "_sys_gt" _sys_gt |>
    Env.add_env_func "_sys_lt" _sys_lt |>
    Env.add_env_func "_sys_add" add |>
    Env.add_env_func "_sys_sub" sub |>
    Env.add_env_func "_sys_mul" mul |>
    Env.add_env_func "_sys_div" div
