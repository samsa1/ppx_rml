(**********************************************************************)
(*                                                                    *)
(*                           ReactiveML                               *)
(*                    http://reactiveML.org                           *)
(*                    http://rml.inria.fr                             *)
(*                                                                    *)
(*                          Louis Mandel                              *)
(*                                                                    *)
(*  Copyright 2002, 2007 Louis Mandel.  All rights reserved.          *)
(*  This file is distributed under the terms of the Q Public License  *)
(*  version 1.0.                                                      *)
(*                                                                    *)
(*  ReactiveML has been done in the following labs:                   *)
(*  - theme SPI, Laboratoire d'Informatique de Paris 6 (2002-2005)    *)
(*  - Verimag, CNRS Grenoble (2005-2006)                              *)
(*  - projet Moscova, INRIA Rocquencourt (2006-2007)                  *)
(*                                                                    *)
(**********************************************************************)

(* file: rmltop.ml *)
(* created: 2005-06-11  *)
(* author: Louis Mandel *)

open Rmltop_compiler

let debug = ref false
let print_DEBUG x =
  if !debug
  then Printf.eprintf x
  else Printf.ifprintf stderr x

let (//) = Filename.concat
let ocaml_stdlib = Filename.dirname !Ocamlbuild_pack.Ocamlbuild_where.libdir

let include_dir = ref [ ocaml_stdlib // "threads"; ]
let hide_rml_dirs = ref false

let include_obj = ref
  ["stdlib.cma";
   "threads.cma";
   "rmllib.cma";
   "rmlrun.cma";
  ]
let add_include_dir inc = include_dir := !include_dir @ [ inc ]
let add_include_obj inc = include_obj := !include_obj @ [ inc ]

let show_help = ref false

let init_rml = [
  "open Implem;;";
]

let print_intro () =
  print_string "        ReactiveML version ";
  print_string Rmlcompiler.Version.version;
  print_newline();
  print_newline();
  if !show_help then print_help ()

let load_dir dir =
  Topdirs.dir_directory dir;
  Rmlcompiler.Configure.add_include dir;
  print_DEBUG "Added %s directory to search path.\n" dir

let load_file file =
  print_DEBUG "Trying to load %s... %!" file;
  if Topdirs.load_file Format.err_formatter file then
  print_DEBUG "done%s%!" "\n"
  else
  Printf.eprintf "Cannot find file %s.\n%!" file

let init_toplevel () =
  List.iter load_dir !include_dir;
  List.iter load_file !include_obj

let load_script file =
  (* TODO *)
  ()

let load_ocamlinit () =
  if Sys.file_exists ".ocamlinit" then
    load_script ".ocamlinit"
  else
    try
      let home_init = Filename.concat (Sys.getenv "HOME") ".ocamlinit" in
      if Sys.file_exists home_init then load_script home_init
    with Not_found -> ()

let print_prompt () = print_string "# "

let main () =
  if not !hide_rml_dirs then begin
    let rml_stdlib = Rmlcompiler.Configure.locate_stdlib () in
    include_dir :=
         rml_stdlib
      :: rml_stdlib // "toplevel"
      :: !include_dir
  end;
  print_intro();
  Sys.catch_break true;
  Rmlcompiler.Misc.interactive := true;
  Rmlcompiler.Misc.print_type := true;
  Toploop.set_paths ();
  Toploop.initialize_toplevel_env ();
  init_toplevel ();
  Rmlcompiler.Interactive.init ();
  Rmlcompiler.Misc.opt_iter Rmltop_global.set_sampling !sampling;
  eval_phrases Format.std_formatter !debug init_rml;
  try
    let buf = Buffer.create 512 in
    print_prompt ();
    while true do
      let line = read_line () in
      let len = String.length line in
      let tail = if len < 2 then "" else String.sub line (len-2) 2 in
      let line = String.concat "" [ line; "\n" ] in
      if tail = ";;" then begin
        let () = Buffer.add_string buf line in
        let phrase = Buffer.contents buf in
        Buffer.reset buf; Buffer.clear buf;
        eval Format.std_formatter Rmltop_compiler.parse (String.copy phrase);
        print_prompt ();
      end
      else if line <> "\n" then begin
        Buffer.add_string buf line
      end;
    done
  with
    | End_of_file -> exit 0
    | Sys.Break -> Printf.eprintf "Interrupted.\n%!"
    | exn -> Printf.eprintf "%s\n%!" (Printexc.to_string exn)

let _ =
  Arg.parse (Arg.align
    [ "-sampling", Arg.Float (fun x -> if x >= 0.0 then sampling := Some x),
      "<rate> Sets the sampling rate to <rate> seconds";
      "-i", Arg.Set show_help, " List known rml directives at startup";
      "-n", Arg.Set hide_rml_dirs, " Do not include RML paths at startup";
      "-debug", Arg.Set debug, " Enable debug output";
      "-I", Arg.String add_include_dir, "<dir>  Add <dir> to the list of include directories";
    ])
    add_include_obj
    "";
  main ()
