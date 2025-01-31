exception Parse_error of Lexing.position * string

let use_stdlib = ref true
let run_repl = ref false  (* Flaga dla REPL-a *)
let fname = ref None

let cmd_args_options =
  [ "-no-stdlib", Arg.Clear use_stdlib, "Do not include stdlib";
    "-r", Arg.Set run_repl, "Run in REPL mode"  (* Dodajemy opcję REPL-a *)
  ]

let usage_string =
  Printf.sprintf "Usage: %s [FILE] or -r" Sys.argv.(0)

let set_fname arg =
  match !fname with
  | None   -> fname := Some arg
  | Some _ ->
    Arg.usage cmd_args_options usage_string;
    exit 2

let run_parser fname (lexbuf : Lexing.lexbuf) =
  Lexing.set_filename lexbuf fname;
  try Parser.prog Lexer.token lexbuf with
  | Parser.Error ->
    raise (Parse_error(Lexing.lexeme_start_p lexbuf, Lexing.lexeme lexbuf))

(* Główna funkcja programu *)
let _ =
  Arg.parse cmd_args_options set_fname usage_string;
  match !fname, !run_repl with
  | None, true -> Repl.run ()  (* Jeśli podano `-r`, uruchom REPL *)
  | Some fname, _ ->
    In_channel.with_open_text fname
      (fun chan ->
        Lexing.from_channel chan
        |> run_parser fname
        |> Eval.eval_prog
        |> ignore)
  | None, false ->
    Arg.usage cmd_args_options usage_string;
    exit 2

  (* with
  | Eval.MyExn ->
    Printf.eprintf "Unhandled exception!\n";
    exit 1
  | Eval.Type_error ->
    Printf.eprintf "type error!\n";
    exit 1
  | Eval.Unbound_var x ->
    Printf.eprintf "unbound variable: %s\n" x;
    exit 1
  | Parse_error(pos, tok) ->
    Printf.eprintf "%s:%d:%d: Syntax error, unexpected token %s\n"
      pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)
      tok;
    exit 1 *)
