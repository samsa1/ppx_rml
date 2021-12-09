open Parse_ast
open Ppxlib
open Parse_ident
open Rml_asttypes

let simple_ident_of_string_loc str =
    {psimple_id = str.txt; psimple_loc = str.loc}

let immediate_of_constant ~loc = function
  | Pconst_integer (str, char_op) -> begin 
      let () = if char_op <> None
        then Location.raise_errorf ~loc "Character at the end of an integer are not supported"
      in Const_int (int_of_string str)
  end
  | Pconst_char c -> Const_char c
  | Pconst_string (s, loc, sop) ->
    let () = if sop <> None
      then Location.raise_errorf ~loc "Unsupported expression"
    in Const_string s
  | Pconst_float (s, char_op) -> begin
      let () = if char_op <> None
        then Location.raise_errorf ~loc "Character at the end of a float are not supported"
      in Const_float (float_of_string s)
  end

let simple_ident_of_pat patt =
  let aux = function
    | Ppat_var s -> s.txt
    | _ -> Location.raise_errorf ~loc:patt.ppat_loc "Invalid syntax"
  in {psimple_loc = patt.ppat_loc; psimple_id = aux patt.ppat_desc}

let sident_typeoptL_of_patt patt =
  match patt.ppat_desc with
    | Ppat_var str -> [simple_ident_of_string_loc str, None]
    | Ppat_tuple pattl -> List.map (fun patt -> (simple_ident_of_pat patt, None)) pattl
    | _ -> Location.raise_errorf ~loc:patt.ppat_loc "Invalid syntax"

let ident_of_lident lident = 
  let pident_id = match lident.txt with
    | Lident s -> Pident s
    | Ldot (Lident s1, s2) -> Pdot (s1, s2)
    | Ldot _ -> Location.raise_errorf ~loc:lident.loc "Invalid syntax"
    | Lapply (_, _) -> Location.raise_errorf ~loc:lident.loc "Invalid syntax"
  in {pident_id; pident_loc = lident.loc}

let rec translate_core_type ctype = 
  let pte_desc = match ctype.ptyp_desc with
  | Ptyp_any -> assert false
  | Ptyp_var str -> RmlPtype_var str
  | Ptyp_arrow (arg_label, ctype1, ctype2) ->
    let () = if arg_label <> Nolabel
      then Location.raise_errorf ~loc:ctype.ptyp_loc "Labelled functions are not supported in rml"  
    in RmlPtype_arrow (translate_core_type ctype1, translate_core_type ctype2)
  | Ptyp_tuple ctypel -> RmlPtype_tuple (List.map translate_core_type ctypel)
  | Ptyp_constr (lident, ctypel) ->
      begin match lident.txt, ctypel with
      | Lident "process", [] -> assert false 
      | Lident "process", [ctype] -> RmlPtype_process (translate_core_type ctype, Def_static.Dontknow)
      | Lident "process", _ -> RmlPtype_process ({pte_desc = RmlPtype_tuple (List.map translate_core_type ctypel); pte_loc = lident.loc}, Def_static.Dontknow) 
      | Lident "+", [{ptyp_desc = Ptyp_constr ({txt = Lident "process"; _}, [ctype2]);_}] ->
        RmlPtype_process (translate_core_type ctype2, Def_static.Noninstantaneous)
      | Lident "+", [{ptyp_desc = Ptyp_constr ({txt = Lident "process"; _}, ctypel2);_}] ->
        RmlPtype_process ({pte_desc = RmlPtype_tuple (List.map translate_core_type ctypel2); pte_loc = lident.loc}, Def_static.Noninstantaneous)
      | Lident "-", [{ptyp_desc = Ptyp_constr ({txt = Lident "process"; _}, [ctype2]);_}] ->
        RmlPtype_process (translate_core_type ctype2, Def_static.Instantaneous)
      | Lident "-", [{ptyp_desc = Ptyp_constr ({txt = Lident "process"; _}, ctypel2);_}] ->
        RmlPtype_process ({pte_desc = RmlPtype_tuple (List.map translate_core_type ctypel2); pte_loc = lident.loc}, Def_static.Instantaneous)
      | _ -> RmlPtype_constr (ident_of_lident lident, List.map translate_core_type ctypel)
      end
  | Ptyp_object _ | Ptyp_class _ | Ptyp_alias _ | Ptyp_variant _
  | Ptyp_poly _ | Ptyp_package _ | Ptyp_extension _ ->
    Location.raise_errorf ~loc:ctype.ptyp_loc "Unsupported type in rml"
  in {pte_desc; pte_loc = ctype.ptyp_loc}

let get_immediate expr = match expr.pexp_desc with
  | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "immediate"; _}; _}, arglabel_expr_list) ->
    (Immediate,
    match arglabel_expr_list with
      | [] -> assert false (* Should not happen *)
      | [(Nolabel, expr)] -> expr
      | (Nolabel, expr2)::tl -> {expr with pexp_desc = Pexp_apply (expr2, tl)}
      | (_, {pexp_loc; _})::_ -> Location.raise_errorf ~loc:pexp_loc "Labelled arguments are not allowed in rml"
    )
  | _ -> (Nonimmediate, expr)

let get_when ~loc exprl =
  let expr_of_exprl ~loc = function
    | [] -> Location.raise_errorf ~loc "Expected expression on both sides of then when operator"
    | [(Nolabel, expr)] -> expr
    | (Nolabel, expr)::tl ->
        {pexp_desc = Pexp_apply (expr, tl); pexp_loc = expr.pexp_loc;
        pexp_attributes = []; pexp_loc_stack = []}
    | (_, {pexp_loc; _})::_ -> Location.raise_errorf ~loc:pexp_loc "Labelled arguments are not allowed in rml"
  in let rec get_when_inner = function
    | [] -> ([], None)
    | (Nolabel, {pexp_desc = Pexp_ident {txt = Lident "when"; _}; _})::tl ->
        ([], Some (expr_of_exprl ~loc tl))
    | element::tl ->
      let (v, s) = get_when_inner tl in
      (element::v , s)
  in
  let (eventl, when_expr) = get_when_inner exprl in
  (expr_of_exprl ~loc eventl, when_expr)

let get_one_when ~loc expr = match expr.pexp_desc with
  | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "one"; _}; _}, arglabel_expr_list) ->
    begin
      match arglabel_expr_list with
        | [] -> assert false (* Should not happen *)
        | [(label, expr)] -> let () = assert (label = Nolabel) in (One, expr, None)
        | _ ->
            let event, when_expr = get_when ~loc arglabel_expr_list in
            (One, event, when_expr)
    end
  | _ -> (All, expr, None)


let get_imm_one_when expr = match expr.pexp_desc with
  | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "immediate"; _}; _}, arglabel_expr_list) ->
    begin
      match arglabel_expr_list with
        | [] -> assert false (* Should not happen *)
        | [(label, expr)] -> let () = assert (label = Nolabel) in (Immediate, All, expr, None)
        | (label, expr2)::tl ->
            let () = assert (label = Nolabel) in
            let await_kind, event, when_expr = get_one_when ~loc:expr.pexp_loc {expr with pexp_desc = Pexp_apply (expr2, tl)} in
            (Immediate, await_kind, event, when_expr)
    end
  | _ -> 
    let await_kind, event, when_expr = get_one_when ~loc:expr.pexp_loc expr in
  (Nonimmediate, await_kind, event, when_expr)

let rec translate_patt patt =
  let ppatt_desc = match patt.ppat_desc with
    | Ppat_any -> Ppatt_any
    | Ppat_var str -> Ppatt_var (simple_ident_of_string_loc str)
    | Ppat_alias (patt, name) -> Ppatt_alias (translate_patt patt, simple_ident_of_string_loc name)
    | Ppat_constant c -> Ppatt_constant (immediate_of_constant ~loc:patt.ppat_loc c)
    | Ppat_tuple pattl -> Ppatt_tuple (List.map translate_patt pattl)
    | Ppat_construct (lident, namel_patt_opt) ->
      begin match (lident.txt, namel_patt_opt) with
        | (Lident "()", None) -> Ppatt_constant Const_unit
        | (Lident "true", None) -> Ppatt_constant (Const_bool true)
        | (Lident "false", None) -> Ppatt_constant (Const_bool false)
        | (_, None) -> Ppatt_construct (ident_of_lident lident, None)
        | (_, Some p) -> Ppatt_construct (ident_of_lident lident, Some (translate_patt p))
      end
    | Ppat_record (lident_patt_l, Closed) -> Ppatt_record (List.map (fun (lident, patt) -> (ident_of_lident lident, translate_patt patt)) lident_patt_l)
    | Ppat_record (_lident_patt_l, Open) -> Location.raise_errorf ~loc:patt.ppat_loc "Unsupported opened record in rml"
    | Ppat_array pattl -> Ppatt_array (List.map translate_patt pattl)
    | Ppat_or (patt1, patt2) -> Ppatt_or (translate_patt patt1, translate_patt patt2)
    | Ppat_constraint (patt, ctype) -> Ppatt_constraint (translate_patt patt, translate_core_type ctype)
    | Ppat_interval _ | Ppat_variant _ | Ppat_type _ | Ppat_lazy _
    | Ppat_unpack _ | Ppat_exception _ | Ppat_extension _ | Ppat_open _ ->
      Location.raise_errorf ~loc:patt.ppat_loc "Unsupported pattern in rml"
  in {ppatt_desc; ppatt_loc = patt.ppat_loc}

let rec pat_expr_of_value_binding vb =
  let rec add_process expr =
    let pexpr_desc = match expr.pexp_desc with
      | Pexp_fun (arg_l, exprop, patt, expr) ->
        let () = assert (arg_l = Nolabel && exprop = None) in
        Pexpr_function [translate_patt patt, None, add_process expr]
      | Pexp_constraint (expr, ctype) ->
        Pexpr_constraint (add_process expr, translate_core_type ctype)
      | _ -> Pexpr_process (translate_expr expr)
    in {pexpr_desc; pexpr_loc = expr.pexp_loc}
  in
    if match vb.pvb_pat.ppat_desc with
    | Ppat_var v -> v.txt = "process"
    | _ -> false
  then
    let patt, new_expr = match vb.pvb_expr.pexp_desc with
      | Pexp_fun (Nolabel, None, patt, expr) -> (translate_patt patt, expr)
      | _ -> assert false
    in (patt, add_process new_expr)
  else
  (translate_patt vb.pvb_pat,
  translate_expr vb.pvb_expr)
and translate_expropt = function
  | None -> None
  | Some expr -> Some (translate_expr expr)
and pat_expop_exp_of_case case =
  (translate_patt case.pc_lhs, translate_expropt case.pc_guard, translate_expr case.pc_rhs)
and event_of_expr expr bindop =
    let pconf_desc = match expr.pexp_desc with
      | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "||"; _}; _}, [(Nolabel, e1); (Nolabel, e2)])
        -> Pconf_or (event_of_expr e1 bindop, event_of_expr e2 bindop)
      | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "&&"; _}; _}, [(Nolabel, e1); (Nolabel, e2)])
        -> Pconf_and (event_of_expr e1 bindop, event_of_expr e2 bindop)
      | _ -> Pconf_present (translate_expr expr, bindop)
    in {pconf_desc; pconf_loc = expr.pexp_loc}
and event_of_patt_ext_event patt = match patt.ppat_desc with
  | Ppat_extension ({txt = "event"; _}, PStr [{pstr_desc = Pstr_eval (expr, []); _}]) ->
    event_of_expr expr None
  | Ppat_extension ({txt = "event"; _}, PStr [{pstr_desc = Pstr_value (Nonrecursive, [vb]); _}]) ->
    event_of_expr vb.pvb_expr (Some (translate_patt vb.pvb_pat))
  | _ -> assert false
and translate_expr expr =
  let pexpr_desc = match expr.pexp_desc with
    | Pexp_ident longident_loc ->
      begin match longident_loc.txt with
        | Lident "pause" -> Pexpr_pause
        | Lident "halt" -> Pexpr_halt
        | Lident "nothing" -> Pexpr_nothing
        | Lident "emit" | Lident "process" -> assert false
        | _ -> Pexpr_ident (ident_of_lident longident_loc)
      end
    | Pexp_constant c ->
      Pexpr_constant (immediate_of_constant ~loc:expr.pexp_loc c)
    | Pexp_let (rf, vbl, expr) -> Pexpr_let (rf, List.map pat_expr_of_value_binding vbl, translate_expr expr)
    | Pexp_function cases -> Pexpr_function (List.map pat_expop_exp_of_case cases)
    | Pexp_fun (arg_l, exprop, patt, expr) ->
        let () = assert (arg_l = Nolabel && exprop = None) in
        Pexpr_function [translate_patt patt, None, translate_expr expr]
    | Pexp_apply (expr, arglabel_expr_list) ->
      begin match expr.pexp_desc with
        | Pexp_ident {txt = Lident "emit"; _} ->
          begin match arglabel_expr_list with
            | [] -> assert false (* should never happen *)
            | [(Nolabel, signal_expr)] -> Pexpr_emit (translate_expr signal_expr)
            | [(Nolabel, signal_expr); (Nolabel, info_expr)] -> Pexpr_emit_val (translate_expr signal_expr, translate_expr info_expr)
            | _ -> assert false
          end
        | Pexp_ident {txt = Lident "run"; _} ->
          begin match arglabel_expr_list with
            | [] -> assert false (* should never happen *)
            | [(Nolabel, proc)] -> Pexpr_run (translate_expr proc)
            | _ -> assert false
          end
        | Pexp_ident {txt = Lident "||"; _} ->
          begin match arglabel_expr_list with
            | [(Nolabel, e1); (Nolabel, e2)] -> Pexpr_par (translate_expr e1, translate_expr e2)
            | _ -> assert false
          end
        | _ -> Pexpr_apply (translate_expr expr, List.map (fun (al, expr) -> let () = assert (al = Nolabel) in translate_expr expr) arglabel_expr_list)
        (* TODO implemented Pexpr_merge = expr |> expr *)
      end
    | Pexp_match (expr, cases) ->
      Pexpr_match (translate_expr expr, List.map pat_expop_exp_of_case cases)
    | Pexp_try (expr, cases) -> Pexpr_trywith (translate_expr expr, List.map pat_expop_exp_of_case cases)
    | Pexp_tuple exprl -> Pexpr_tuple (List.map translate_expr exprl)
    | Pexp_construct (ident, expop) ->
      begin match ident, expop with
        | ({txt = Lident "()"; _}, None) -> Pexpr_constant Const_unit
        | ({txt = Lident "true"; _}, None) -> Pexpr_constant (Const_bool true)
        | ({txt = Lident "false"; _}, None) -> Pexpr_constant (Const_bool false)
        | _ -> Pexpr_construct (ident_of_lident ident, translate_expropt expop)
      end
    | Pexp_variant _ -> let () = assert false in Pexpr_ocaml expr
    | Pexp_record (name_expr_list, expop) ->
      let ident_expr_list = List.map (fun (name, expr) -> (ident_of_lident name, translate_expr expr)) name_expr_list in
      begin match expop with
        | None -> Pexpr_record ident_expr_list
        | Some e -> Pexpr_record_with (translate_expr e, ident_expr_list)
      end
    | Pexp_field (expr, lident) -> Pexpr_record_access (translate_expr expr, ident_of_lident lident)
    | Pexp_setfield (expr_mod, lident, expr_val) -> Pexpr_record_update (translate_expr expr_mod, ident_of_lident lident, translate_expr expr_val)
    | Pexp_array exprl -> Pexpr_array (List.map translate_expr exprl)
    | Pexp_ifthenelse (exprif, exprthen, exprelse) -> Pexpr_ifthenelse (translate_expr exprif, translate_expr exprthen, translate_expropt exprelse)
    | Pexp_sequence (expr1, expr2) -> Pexpr_seq (translate_expr expr1, translate_expr expr2)
    | Pexp_while ({pexp_desc = Pexp_construct ({txt = Lident "true"; _}, None); _}, block) -> Pexpr_loop (translate_expr block)
    | Pexp_while (cond, block) -> Pexpr_while (translate_expr cond, translate_expr block)
    | Pexp_for (pat, expr1, expr2, dir, expr3) -> Pexpr_for (simple_ident_of_pat pat, translate_expr expr1, translate_expr expr2, dir, translate_expr expr3)
    | Pexp_constraint (expr, ctype) -> Pexpr_constraint (translate_expr expr, translate_core_type ctype)
    | Pexp_coerce _ -> let () = assert false in Pexpr_ocaml expr
    | Pexp_send _ -> let () = assert false in Pexpr_ocaml expr
    | Pexp_new _ -> let () = assert false in Pexpr_ocaml expr
    | Pexp_setinstvar _ -> let () = assert false in Pexpr_ocaml expr
    | Pexp_override _ -> let () = assert false in Pexpr_ocaml expr
    | Pexp_letmodule _ -> let () = assert false in Pexpr_ocaml expr
    | Pexp_letexception _ -> let () = assert false in Pexpr_ocaml expr
    | Pexp_assert expr -> Pexpr_assert (translate_expr expr)
    | Pexp_lazy expr -> let () = assert false in Pexpr_ocaml expr
    | Pexp_poly _ -> let () = assert false in Pexpr_ocaml expr
    | Pexp_object _ -> let () = assert false in Pexpr_ocaml expr
    | Pexp_newtype _ -> let () = assert false in Pexpr_ocaml expr
    | Pexp_pack _ -> let () = assert false in Pexpr_ocaml expr
    | Pexp_open _ -> let () = assert false in Pexpr_ocaml expr
    | Pexp_letop _ -> let () = assert false in Pexpr_ocaml expr
    | Pexp_extension (name, payload) -> begin
      match name.txt, payload with
        | "para", PStr [stri] | "par", PStr [stri] -> begin
            match stri.pstr_desc with
              | Pstr_eval (expr, []) ->
                begin match expr.pexp_desc with
                  | Pexp_for (pat, expr1, expr2, dir, expr3) -> Pexpr_fordopar (simple_ident_of_pat pat, translate_expr expr1, translate_expr expr2, dir, translate_expr expr3)
                  | _ -> assert false
                end
              | _ -> assert false
            end
        | "await", PStr [stri] -> begin
          match stri.pstr_desc with
            | Pstr_eval ({pexp_desc = Pexp_let (Nonrecursive, [vb], in_expr); _} as expr, []) ->
                begin match vb.pvb_pat.ppat_desc with
                  | Ppat_construct ({txt = Lident "()"; _}, None) ->
                      let imm, event = get_immediate vb.pvb_expr in
                      Pexpr_seq (
                        {pexpr_desc = Pexpr_await (imm, event_of_expr event None);
                       pexpr_loc = expr.pexp_loc;},
                       translate_expr in_expr)
                  | _ ->
                      let imm, await_kind, event, when_expr = get_imm_one_when vb.pvb_expr in
                      Pexpr_await_val (imm, await_kind, event_of_expr event (Some (translate_patt vb.pvb_pat)), translate_expropt when_expr, translate_expr in_expr)
                end              
            | _ -> assert false
          end
        | "signal", PStr [stri] ->
            begin match stri.pstr_desc with
              | Pstr_eval ({pexp_desc = Pexp_let (Nonrecursive, [vb], in_expr); _}, []) ->
                begin match vb.pvb_expr.pexp_desc with
                | Pexp_construct ({txt = Lident "()"; _}, None) -> Pexpr_signal (sident_typeoptL_of_patt vb.pvb_pat, None, translate_expr in_expr)
                | Pexp_record ([({txt = Lident "default"; _}, expr_default); ({txt = Lident "gather"; _}, expr_gather)], None)
                | Pexp_record ([({txt = Lident "gather"; _}, expr_gather); ({txt = Lident "default"; _}, expr_default)], None) ->
                  Pexpr_signal (sident_typeoptL_of_patt vb.pvb_pat, Some (Default, translate_expr expr_default, translate_expr expr_gather), translate_expr in_expr)
                | Pexp_record ([({txt = Lident "memory"; _}, expr_memory); ({txt = Lident "gather"; _}, expr_gather)], None)
                | Pexp_record ([({txt = Lident "gather"; _}, expr_gather); ({txt = Lident "memory"; _}, expr_memory)], None) ->
                    Pexpr_signal (sident_typeoptL_of_patt vb.pvb_pat, Some (Memory, translate_expr expr_memory, translate_expr expr_gather), translate_expr in_expr)
                | Pexp_record _ -> Pexpr_signal (sident_typeoptL_of_patt vb.pvb_pat, None, translate_expr in_expr)
                | _ -> assert false
                end
              | _ -> assert false 
            end
        | "until", PStr [stri] ->
          begin match stri.pstr_desc with
            | Pstr_eval ({pexp_desc = Pexp_try (expr, cases); _}, []) ->
              Pexpr_until (translate_expr expr,
                List.map (fun case -> (event_of_patt_ext_event case.pc_lhs, translate_expropt case.pc_guard, Some (translate_expr case.pc_rhs))) cases)
            | _ -> assert false
          end
        | _, _ -> let () = print_endline ("extension "^name.txt^" not implemented") in assert false
      end
    | Pexp_unreachable -> let () = assert false in Pexpr_ocaml expr
  in {pexpr_desc; pexpr_loc = expr.pexp_loc}

let sident_strlist_tdecl_of_tdecl ptype =
  let sident = simple_ident_of_string_loc ptype.ptype_name in
  let () = assert (ptype.ptype_manifest = None) in
  let () = assert (ptype.ptype_params = []) in
  let () = assert (ptype.ptype_cstrs = []) in
  let () = assert (ptype.ptype_attributes = []) in
  let () = assert (ptype.ptype_params = []) in
  let tdecl = match ptype.ptype_kind with
    | Ptype_abstract -> assert false
    | Ptype_variant const_declL ->
        RmlPtype_variant (List.map (fun _ -> assert false) const_declL)
    | Ptype_record label_declL ->
        RmlPtype_record (List.map
                          (fun label_decl ->
                            (simple_ident_of_string_loc label_decl.pld_name,
                             label_decl.pld_mutable,
                             translate_core_type label_decl.pld_type))
                          label_declL)
    | Ptype_open -> assert false
  in (sident, [], tdecl)

let impl_item_of_str_item stri =
  let pimpl_desc = match stri.pstr_desc with
    | Pstr_eval (expr, attributes) ->
      let () = assert ([] = attributes) in
      Pimpl_expr (translate_expr expr)
    | Pstr_value (rec_flag, vbs) ->
      Pimpl_let (rec_flag, List.map pat_expr_of_value_binding vbs)
    | Pstr_primitive _value_description -> assert false
    | Pstr_type (_, type_declaration_list) ->
        Pimpl_type (List.map sident_strlist_tdecl_of_tdecl type_declaration_list)
    | Pstr_typext _type_extension -> assert false
    | Pstr_exception _type_exception -> assert false
    | Pstr_module _module_binding -> assert false
    | Pstr_recmodule _module_binding_list -> assert false
    | Pstr_modtype _module_type_declaration -> assert false
    | Pstr_open _open_declaration -> assert false
    | Pstr_class _class_declaration_list -> assert false
    | Pstr_class_type _class_type_declaration_list -> assert false
    | Pstr_include _include_declaration -> assert false
    | Pstr_attribute _attribute -> assert false
    | Pstr_extension ((name, payload), attributes) ->
        let () = assert (attributes = []) in
        begin match (name.txt, payload) with
          | "para", PStr [stri] | "par", PStr [stri] ->
            begin match stri.pstr_desc with
              | Pstr_eval _ -> assert false
              | Pstr_value (_rec_flag, _vb_list) -> assert false
              | _ -> assert false
            end
          | "signal", PStr [stri] ->
            begin match stri.pstr_desc with
              | Pstr_value (_rec_flag, [vb]) ->
                begin match vb.pvb_expr.pexp_desc with
                | Pexp_construct ({txt = Lident "()"; _}, None) -> Pimpl_signal (sident_typeoptL_of_patt vb.pvb_pat, None)
                | Pexp_record ([({txt = Lident "default"; _}, expr_default); ({txt = Lident "gather"; _}, expr_gather)], None)
                | Pexp_record ([({txt = Lident "gather"; _}, expr_gather); ({txt = Lident "default"; _}, expr_default)], None) ->
                  Pimpl_signal (sident_typeoptL_of_patt vb.pvb_pat, Some (Default, translate_expr expr_default, translate_expr expr_gather))
                | Pexp_record ([({txt = Lident "memory"; _}, expr_memory); ({txt = Lident "gather"; _}, expr_gather)], None)
                | Pexp_record ([({txt = Lident "gather"; _}, expr_gather); ({txt = Lident "memory"; _}, expr_memory)], None) ->
                    Pimpl_signal (sident_typeoptL_of_patt vb.pvb_pat, Some (Memory, translate_expr expr_memory, translate_expr expr_gather))
                | Pexp_record _ -> Pimpl_signal (sident_typeoptL_of_patt vb.pvb_pat, None)
                | _ -> assert false
                end
              | _ -> assert false
            end 
          | _, _ -> assert false
        end

  in {pimpl_desc; pimpl_loc = stri.pstr_loc}
  

let main ~loc:_ ~path:_ str_item_list =
  List.map impl_item_of_str_item str_item_list
