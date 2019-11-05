module Config = struct
  let verbose = ref false
end

let cmd_opts =
  let open Arg in
  align [
    "-v", Set Config.verbose, " Verbose output" ;
  ]

let process_file f =
  assert (Filename.check_suffix f ".bx") ;
  let ch = open_in f in
  let lb = Lexing.from_channel ch in
  lb.lex_curr_p <- {lb.lex_curr_p with pos_fname = f} ;
  try begin
    let prog = Parser.prog Lexer.token lb in
    if !Config.verbose then begin
      Format.printf "---- processed & type-checked AST ---@." ;
      Ast.pp_prog Format.std_formatter prog ;
      Format.printf "==== OUTPUT ====@."
    end ;
    let _ =
      Interpret.interpret_proc_call
        {globals = prog ; locals = Utils.StringMap.empty}
        "main" [] in
    ()
  end with
  | Ast.Typecheck msg ->
      Format.eprintf "Type checking failure: %s@." msg
  | Interpret.Execution_failure msg ->
      Format.eprintf "Execution failure: %s@." msg
  | Parser.Error ->
      let open Lexing in
      Format.eprintf "Parse error at %S:%d.%d@."
        lb.lex_curr_p.pos_fname
        lb.lex_curr_p.pos_lnum
        (lb.lex_curr_p.pos_cnum - lb.lex_curr_p.pos_bol)

let main () =
  let files = ref [] in
  Arg.parse cmd_opts
    (fun f -> files := f :: !files)
    (Printf.sprintf "Usage: %s [OPTIONS] file1 ..."
       (Filename.basename Sys.executable_name)) ;
  List.iter process_file (List.rev !files)

let () =
  if not !Sys.interactive then
    main ()
