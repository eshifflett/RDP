open Parser
open Lexer
open SmallCTypes
open TokenTypes

(* Buffers for testing prints *)
let (print_buffer : Buffer.t) = Buffer.create 100
let flush_print_buffer () : unit = Buffer.clear print_buffer;;
let assert_buffer_equal (s : string) : unit =
  let c = Buffer.contents print_buffer in
  if not (s = c) then failwith ("Printed: '" ^ c ^ "' Expected:'" ^ s ^ "'");;

(* This is the print you must use for the project *)
let print_output_string (s : string) : unit =
  Buffer.add_string print_buffer s;
  Stdlib.print_string s

let print_output_int (i : int) : unit =
  print_output_string (string_of_int i)

let print_output_bool (b : bool) : unit =
  print_output_string (string_of_bool b)

let print_output_newline () : unit =
  print_output_string "\n"

(* Removes shadowed bindings from an execution environment *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Eval report print function *)
let print_eval_env_report (env : environment): unit =

  print_string "*** BEGIN POST-EXECUTION ENVIRONMENT REPORT ***\n";

  List.iter (fun (var, value) ->
      let vs = begin match value with
        | Int_Val(i) -> string_of_int i
        | Bool_Val(b) -> string_of_bool b
      end in

      Printf.printf "- %s => %s\n" var vs) (prune_env env);

  print_string "***  END POST-EXECUTION ENVIRONMENT REPORT  ***\n"
