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

let get_constant ~loc msg = function
  | PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant c; _}, _); _}] -> c
  | _ -> Location.raise_errorf ~loc msg

let get_int ~loc structure = 
  match get_constant ~loc "Awaits an integer" structure with
    | Pconst_integer (str, None) -> int_of_string str
    | _ -> Location.raise_errorf ~loc "Awaits an integer"

let get_str ~loc structure = 
  match get_constant ~loc "Awaits a string" structure with
    | Pconst_string (str, _, None) -> str
    | _ -> Location.raise_errorf ~loc "Awaits a string"

let get_float ~loc structure = 
  match get_constant ~loc "Awaits a string" structure with
    | Pconst_float (str, None) -> float_of_string str
    | _ -> Location.raise_errorf ~loc "Awaits a float"

let rec compute_args = function
  | {pstr_desc = Pstr_attribute attribute; pstr_loc = loc}::tl ->
      let () = match attribute.attr_name.txt with
        | "stdlib" -> Options.stdlib := Some (get_str ~loc attribute.attr_payload)
        | "v" -> Options.v := true
        | "version" -> Options.version := true
        | "where" -> Options.where := true
        | "c" -> no_link := true
        | "I" -> Configure.add_include (get_str ~loc attribute.attr_payload)
        | "s" -> Configure.set_simulation_process (get_str ~loc attribute.attr_payload)
        | "n" -> Configure.set_number_of_instant (get_int ~loc attribute.attr_payload)
        | "sampling" -> Configure.set_sampling (get_float ~loc attribute.attr_payload)
        | "i" -> Configure.set_verbose ()
        | "annot" -> Configure.set_save_types ()
        | "dtypes" -> Configure.set_save_types ()
        | "no_reactivity_warning" -> Configure.unset_reactivity_warning ()
        | "dreactivity" -> Configure.set_dreactivity ()
        | "no_reactivity_simpl" -> Configure.unset_no_reactivity_simpl ()
        | "old_loop_warning" -> Configure.set_old_instantaneous_loop_warning ()
        | "runtime" -> Options.runtime := get_str ~loc attribute.attr_payload
        | "thread" -> Rml_misc.with_thread := true
        | "debug" -> Rml_misc.with_debug := true
        | "interactive" -> Configure.set_interactive ()
        | "d" -> Configure.set_output_dir (get_str ~loc attribute.attr_payload)
        | "nostdlib" -> Configure.set_no_stdlib ()
        | "no_nary_opt" -> Configure.set_no_nary ()
        | "no_static_opt" -> Configure.set_no_static ()
        | "no_for_opt" -> Configure.set_no_for ()
        | "no_const_opt" -> Location.raise_errorf ~loc "Option not implemented"
        | "dparse" -> Configure.set_dparse ()
        | "dtime" -> Configure.set_dtime ()
        | _ -> Location.raise_errorf ~loc "Unknown option"
      in compute_args tl
  | x -> let () = Options.eval_options () in x


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
            {pmod_desc = Pmod_structure (Compiler.translate_implementation ~loc ~path (compute_args str));
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