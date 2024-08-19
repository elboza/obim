open Printf

let usage_msg = "pfatt [-i intUpperBound] [-l lines] [-n itemsInRow]"

let ffile = ref ""
let fdebug=ref false
let finteractive=ref false
let ffiles = ref []
let fstdin=ref false
let fexec=ref ""
let anon_fun argp =
  ffiles := argp :: !ffiles

let speclist =
  [("-f", Arg.Set_string ffile, "process file");
  ("-i", Arg.Set finteractive, "interactive shell");
  ("-s", Arg.Set fstdin, "process stdin");
  ("-x", Arg.Set_string fexec, "exec string");
  ("-d", Arg.Set fdebug, "debug")]

let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

let main =
  Arg.parse speclist anon_fun usage_msg;
  if !fdebug=true then
   begin
      printf "fdebug: %B\n" !fdebug;
      printf "ffile: %s\n" !ffile;
      printf "finteractive: %B\n" !finteractive;
      printf "fstdin: %B\n" !fstdin;
      printf "fexec: %s\n" !fexec;
      printf "ffiles: [%s]\n" (String.concat "," !ffiles);    
   end;
  if !fstdin=true then
   begin
        Eval.init_env |> Eval.exec (Lexing.from_channel stdin) |> ignore
   end;

  if !fexec<>"" then
   begin
        Eval.init_env |> Eval.exec (Lexing.from_string !fexec) |> ignore
   end;

  if !ffile<>"" then
   begin
        Eval.init_env |> Eval.exec (Lexing.from_string (read_whole_file !ffile)) |> ignore
   end;

  if !finteractive=true then
   begin
        Repl.repl () |> ignore
   end;

