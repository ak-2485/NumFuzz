(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   Copyright (c) 2018, MINES ParisTech
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)
open Support.Options
open Support.Error

let outfile = ref (None : string option)
let infile = ref ("" : string)

let argDefs =
  [
    ( "-o",
      Arg.String (fun s -> outfile := Some s),
      "File to write the generated code to" );
    ( "-v",
      Arg.Int (fun l -> debug_options := { !debug_options with level = l }),
      "Set printing level to n (1 Warning [2 Info], 3+ Debug)" );
    ( "--verbose",
      Arg.Int (fun l -> debug_options := { !debug_options with level = l }),
      "Set printing level to n (1 Warning [2 Info], 3+ Debug)" );
    ( "--disable-unicode",
      Arg.Unit
        (fun () -> debug_options := { !debug_options with unicode = false }),
      "Disable unicode printing" );
    ( "--enable-annot",
      Arg.Unit
        (fun () -> debug_options := { !debug_options with pr_ann = true }),
      "Enable printing of type annotations" );
    ( "--print-var-full",
      Arg.Unit
        (fun () ->
          debug_options := { !debug_options with var_output = PrVarBoth }),
      "Print names and indexes of variables" );
    ( "--print-var-index",
      Arg.Unit
        (fun () ->
          debug_options := { !debug_options with var_output = PrVarIndex }),
      "Print just indexes of variables" )
  ]

let dp = Support.FileInfo.dummyinfo
let main_error = error_msg General
let main_warning fi = message 1 General fi
let main_info fi = message 2 General fi
let main_info2 fi = message 3 General fi
let main_debug fi = message 4 General fi

let parseArgs () =
  let inFile = ref (None : string option) in
  Arg.parse argDefs
    (fun s ->
      match !inFile with
      | Some _ -> main_error dp "You must specify exactly one input file"
      | None -> inFile := Some s)
    "Usage: bean [options] inputfile";
  match !inFile with
  | None ->
      main_error dp "No input file specified (use --help to display usage info)";
      ""
  | Some s -> s

(* Parse the input *)
let parse file =
  let readme,writeme = Unix.pipe () in
  ignore (Unix.create_process
      "cpp" [|"cpp" ; "-w" ; file |]
      Unix.stdin writeme Unix.stderr);
  Unix.close writeme;
  let pi = Unix.in_channel_of_descr readme in
  let lexbuf = Lexer.create file pi in
  let (context, dcontext, program) =
    try Parser.body Lexer.main lexbuf
    with Parsing.Parse_error ->
      error_msg Parser (Lexer.info lexbuf) "Parse error"
  in
  Parsing.clear_parser ();
  close_in pi;
  (context, dcontext, program)

let type_check program context dcontext =
  let (ty, ctx) = Ty_bi.get_type program context dcontext in
  main_info dp "Type of the program: @[%a@]@\n" Print.pp_type ty;
  if (dcontext != Ctx.empty_context) then
    main_info dp "Discrete Variables:@\n@[%a@]@\n" Print.pp_var_ctx dcontext;
  main_info dp "Inferred Context:@\n@[%a@]@\n" Print.pp_var_ctx_si (List.combine context ctx)

let main () =
  (* Setup the pretty printing engine *)
  let fmt_margin =
    try snd (Util.get_terminal_size ())
    with _ ->
      main_warning dp "Failed to get terminal size value.";
      120
  in

  let set_pp fmt =
    Format.pp_set_ellipsis_text fmt "[...]";
    Format.pp_set_margin fmt (fmt_margin + 1);
    (* Don't ever ask *)
    Format.pp_set_max_indent fmt fmt_margin
  in

  set_pp Format.std_formatter;
  set_pp Format.err_formatter;

  (* Read the command-line arguments *)
  infile := parseArgs ();

  let (context, dcontext, program) = parse !infile in

  (* Print the results of the parsing phase *)
  main_debug dp "Parsed program:@\n@[%a@]@." Print.pp_term program;
  main_debug dp "Parsed discrete context:@\n@[%a@]@." Print.pp_var_ctx dcontext;
  main_debug dp "Parsed linear context:@\n@[%a@]@." Print.pp_var_ctx context;
  main_debug dp "Parsed indices:@\n@[%a@]@." Print.pp_var_ctx_ind context;

  if comp_enabled TypeChecker then type_check program context dcontext

let time f x =
  let t = Sys.time () in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t);
  fx

let res =
  try
    time main ();
    0
  with Exit x -> x

let () = exit res
