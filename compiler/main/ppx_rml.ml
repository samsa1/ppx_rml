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

(* file: main.ml *)

(* Warning: *)
(* This file is based on the original version of main.ml *)
(* from the Lucid Synchrone version 2 distribution, Lip6 *)

(* first modification: 2004-05-06 *)
(* modified by: Louis Mandel      *)

(* $Id$ *)
open Inside_rml
open Ppxlib
open Rml_misc

let () = Options.set_options false
let ext = 
  Extension.declare_with_path_arg
    "rml"
    Extension.Context.structure_item
    Ast_pattern.(pstr __)
    (fun ~loc ~path ~arg str ->
      try
        { pstr_desc =
          Pstr_module
          {pmb_name = {txt = Some (match arg with | None -> "Rml" | Some {txt = Lident str; _} -> str | _ -> assert false ); loc = Location.none};
            pmb_expr =
            {pmod_desc = Pmod_structure (Compiler.translate_implementation ~loc ~path str);
              pmod_loc = Location.none;
              pmod_attributes = [];
            };
            pmb_loc = Location.none;
            pmb_attributes = [];};
          pstr_loc = Location.none;
        }
      with x ->
        Rml_errors.report_error !err_fmt x;
        Format.pp_print_flush !std_fmt ();
        Format.pp_print_flush !err_fmt ();
        exit 2)


let () = Ppxlib.Driver.register_transformation "rml" ~extensions:[ext]




(*
(* list of object files passed on the command line *)
let object_files = ref []

let compile file =
  if Filename.check_suffix file ".rml"
  then
    let filename = Filename.chop_suffix file ".rml" in
    let modname = Filename.basename filename in
    compile_implementation (String.capitalize_ascii modname) filename;
    object_files := modname::!object_files
  else if Filename.check_suffix file ".rmli"
  then
    let filename = Filename.chop_suffix file ".rmli" in
    compile_interface (String.capitalize_ascii (Filename.basename filename))
      filename
  else if Filename.check_suffix file ".mli"
  then
    let filename = Filename.chop_suffix file ".mli" in
    compile_scalar_interface
      (String.capitalize_ascii (Filename.basename filename)) filename
  else
    raise (Arg.Bad ("don't know what to do with " ^ file))


let main () =
  try
    List.iter compile !to_compile
  with x ->
    Errors.report_error !err_fmt x;
    Format.pp_print_flush !std_fmt ();
    Format.pp_print_flush !err_fmt ();
    exit 2
;;

Printexc.catch main ();
(* this is strange, but is required to avoid a bug in ocaml 3.04 *)
Format.set_formatter_out_channel stdout;
if !interactive then Interactive.compile ();
if !dtime then Diagnostic.print !err_fmt;
Format.pp_print_flush !std_fmt ();
Format.pp_print_flush !err_fmt ();
exit 0;;

*)