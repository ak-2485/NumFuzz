(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   Copyright (c) 2018, MINES ParisTech
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)

open Support.Options
open Support.Error
open Constr

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
    ( "--disable-types",
      Arg.Unit (fun () -> comp_disable TypeChecker),
      "Disable type checking and inference" );
    ( "--disable-codegen",
      Arg.Unit (fun () -> comp_disable Backend),
      "Disable code generation" );
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
    "Usage: fuzz [options] inputfile";
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
  let (context, program) =
    try Parser.body Lexer.main lexbuf
    with Parsing.Parse_error ->
      error_msg Parser (Lexer.info lexbuf) "Parse error"
  in
  Parsing.clear_parser ();
  close_in pi;
  (context, program)

let type_check program context =
  let (ty,ctx) = Ty_bi.get_type program context in
  (* main_info dp "Type of the program: @[%a@]" Print.pp_type ty *)
  main_info dp "Inferred Context:@\n@[%a@]@." (Print.pp_list Print.pp_si_op) ctx

let gen_caml program outfile =
  let out = open_out outfile in
  let ofmt = Format.formatter_of_out_channel out in
  Backend.gen_program ofmt program

(* Must use this *)
let get_tty_size = ()

(* === The main function === *)
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

  let (context, program) = parse !infile in

  (* Print the results of the parsing phase *)
  main_debug dp "Parsed program:@\n@[%a@]@."  Print.pp_term program;
  main_debug dp "Parsed context:@\n@[%a@]@."  Print.pp_var_ctx context.var_ctx;
  main_debug dp "Parsed indicies:@\n@[%a@]@." Print.pp_var_ctx_ind context.var_ctx;

  if comp_enabled TypeChecker then type_check program context;

  (if comp_enabled Backend then
     match !outfile with
     | None ->
         main_warning dp
           "No executable was specified, use -o to generate an executable file"
     | Some out_f ->
         let out_ml = out_f ^ ".ml" in
         let out_exe = out_f ^ ".byte" in
         let command =
           "ocamlbuild -I runtime -libs str -cflag '-rectypes' " ^ out_exe
         in

         gen_caml program out_ml;

         main_info dp "Compiling: %s" command;
         let _caml_exit = Sys.command command in
         let _caml_exit = Sys.command "rm -f *.cmo *.cmi" in
         main_info dp "Executable: %s generated" out_exe;
         ())

let time f x =
  let t = Sys.time () in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t);
  fx

(* === Call the main function and catch any exceptions === *)

let res =
  try
    time main ();
    0
  with Exit x -> x

let () = exit res
