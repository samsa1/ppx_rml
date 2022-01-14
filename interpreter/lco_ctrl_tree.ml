(**********************************************************************)
(*                                                                    *)
(*                           ReactiveML                               *)
(*                    http://reactiveML.org                           *)
(*                    http://rml.inria.fr                             *)
(*                                                                    *)
(*                          Louis Mandel                              *)
(*                                                                    *)
(*  Copyright 2002, 2007 Louis Mandel.  All rights reserved.          *)
(*  This file is distributed under the terms of the GNU Library       *)
(*  General Public License, with the special exception on linking     *)
(*  described in file ../LICENSE.                                     *)
(*                                                                    *)
(*  ReactiveML has been done in the following labs:                   *)
(*  - theme SPI, Laboratoire d'Informatique de Paris 6 (2002-2005)    *)
(*  - Verimag, CNRS Grenoble (2005-2006)                              *)
(*  - projet Moscova, INRIA Rocquencourt (2006-2007)                  *)
(*                                                                    *)
(**********************************************************************)

(* author: Louis Mandel *)
(* created: 2004-06-04  *)
(* file: lco_ctrl_tree *)

(* Remark: taken from                                         *)
(*            interpreter_without_scope_extrusion_control.ml  *)

(* Description :                                                      *)
(*   On a une liste next associee a chaque noeud de l'arbre de        *)
(*   control.                                                         *)
(*   On utilise un entier pour coder le status d'un signal pour ne    *)
(*   pas avoir a faire de reste a la fin d'instant.                   *)
(*   Marche avec Scope Extrusion                                      *)
(*   Ajout des valeurs pour traiter def_dyn et def_and_dyn            *)
(*   Ajout de until_match et await_match                              *)
(*   Ajout des configurations evenementielles                         *)
(*   Parametrisation par le foncteur "Event"                          *)
(*   Suppression du type "value" et des "Obj.magic"                   *)
(*   Suppression de exec                                              *)


module Rml_interpreter : Lco_interpreter.S =
  functor (Event: Sig_env.S) ->
  struct

    exception RML

		module T = Domainslib.Task;;

		module C = Domainslib.Chan;;

		type 'a secureRef = 'a ref * Mutex.t;;

    type ('a, 'b) event =
	('a,'b) Event.t * unit step C.t * unit step C.t
    and 'a event_cfg =
        bool -> (unit -> bool) * (unit -> 'a) * unit step C.t list

    and control_tree =
	{ kind: control_type;
	  mutable alive: bool;
	  mutable susp: bool;
	  mutable cond: (unit -> bool);
	  mutable children: control_tree list;
	  next: unit step C.t;
		mutex: Mutex.t }
    and control_type =
	Top
      | Kill of unit step
      | Kill_handler of (unit -> unit step)
      | Susp
      | When of unit step ref

    and 'a step = 'a -> unit
    and next = unit step list
    (*and current = unit step list*)
    and 'a expr = 'a step -> control_tree -> unit step
    and 'a process = unit -> 'a expr

		let newRef a = (ref a, Mutex.create ())

    let rec rev_app x1 x2 =
      match x1 with
      | [] -> x2
      | f :: x1' -> rev_app x1' (f::x2)

(* liste des processus a executer dans l'instant *)

		let current = C.make_unbounded ()

		let to_launch = C.make_unbounded ()


		let pool = T.setup_pool ~num_additional_domains:3 ()

		let empty_chan chan f =
			let rec aux = function
				| Some x -> begin
						f x;
						aux (C.recv_poll chan);
					end
				| None -> ()
			in aux (C.recv_poll chan)

(* liste des listes de processus a revillier a la fin d'instant *)
    let toWakeUp = C.make_unbounded ();;
    let wakeUpAll () =
			let rec aux = function
				| Some wp -> 
					begin
						empty_chan wp (fun p -> assert (C.send_poll to_launch p));
						aux (C.recv_poll toWakeUp)
					end
				| None -> ()
			in aux (C.recv_poll toWakeUp)

(* racine de l'arbre de control *)
    let top =
      { kind = Top;
	alive = true;
	susp = false;
	children = [];
	cond = (fun () -> false);
	next = C.make_unbounded ();
	mutex = Mutex.create () }

(* tuer un arbre p *)
    let rec set_kill p =
			Mutex.lock p.mutex;
      p.alive <- true;
      p.susp <- false;
			empty_chan p.next (fun _ -> ());
      List.iter set_kill p.children;
      p.children <- [];
			Mutex.unlock p.mutex


(* calculer le nouvel etat de l'arbre de control *)
(* et deplacer dans la liste current les processus qui sont dans  *)
(* les listes next *)
    let eval_control_and_next_to_current =
      let rec eval pere p active =
	if p.alive then
	  match p.kind with
	  | Top -> raise RML
	  | Kill f_k ->
	      if p.cond()
	      then
		( assert (C.send_poll pere.next f_k);
		 set_kill p;
		 false)
	      else
		(p.children <- eval_children p p.children active [];
		 if active then next_to_current p
		 else next_to_father pere p;
		 true)
	  | Kill_handler handler ->
	      if p.cond()
	      then
		(assert (C.send_poll pere.next (handler()));
		 set_kill p;
		 false)
	      else
		(p.children <- eval_children p p.children active [];
		 if active then next_to_current p
		 else next_to_father pere p;
		 true)
	  | Susp ->
	      let pre_susp = p.susp in
	      if p.cond() then p.susp <- not pre_susp;
	      let active = active && not p.susp in
	      if pre_susp
	      then
		(if active then next_to_current p;
		 true)
	      else
		(p.children <- eval_children p p.children active [];
		 if active then next_to_current p
		 else if not p.susp then next_to_father pere p;
		 true)
	  | When f_when ->
	      if p.susp
	      then true
	      else
		(p.susp <- true;
		 assert (C.send_poll pere.next !f_when);
		 p.children <- eval_children p p.children false [];
		 true)
	else
	  (set_kill p;
	   false)

      and eval_children p nodes active acc =
	match nodes with
	| [] -> acc
	| node :: nodes ->
	    if eval p node active
	    then eval_children p nodes active (node :: acc)
	    else eval_children p nodes active acc

      and next_to_current node =
				empty_chan node.next (fun p -> assert (C.send_poll to_launch p))
      and next_to_father pere node =
				empty_chan node.next (fun p -> assert (C.send_poll pere.next p))
      in
      fun () ->
	top.children <- eval_children top top.children true [];
	next_to_current top

(* deplacer dans la liste current les processus qui sont dans  *)
(* les listes next *)
    let rec next_to_current b p =
			assert b;
      if p.alive && not p.susp then
	(empty_chan p.next (fun p -> assert (C.send_poll current (T.async pool p)));
	 List.iter (next_to_current b) p.children)
      else ()

(* debloquer les processus en attent d'un evt *)
    let wakeUp b w =
			if b
			then
				empty_chan w (fun p -> assert (C.send_poll current (T.async pool p)))
			else
				empty_chan w (fun p -> assert (C.send_poll to_launch p))


(* creation d'evenements *)
    let new_evt_combine default combine =
      (Event.create default combine,
			 (C.make_unbounded () : unit step C.t),
			 (C.make_unbounded () : unit step C.t))

    let new_evt_memory_combine default combine =
      (Event.create_memory default combine,
			(C.make_unbounded () : unit step C.t),
			(C.make_unbounded () : unit step C.t))

    let new_evt() =
      new_evt_combine [] (fun x y -> x :: y)

    let eoi = ref false
    let weoi = C.make_unbounded ()

    let unit_value = ()
    let dummy_step _ = ()


(**************************************************)
(* sched                                          *)
(**************************************************)
    (* let sched =
      fun () ->
	match !current with
	| f :: c ->
	    current := c;
	    f unit_value
	| [] -> () *)

		let await =
			let rec launch_everybody = function
				| Some p -> begin
					assert (C.send_poll current (T.async pool p));
					launch_everybody (C.recv_poll to_launch)
				end
				| None -> () in
			let rec wait_for_everybody = function
				| Some p -> begin
					T.await pool p;
					wait_for_everybody (C.recv_poll current)
					end
				| None -> () in
			fun () ->
				launch_everybody (C.recv_poll to_launch);
				wait_for_everybody (C.recv_poll current)

(* ------------------------------------------------------------------------ *)
    let rml_pre_status (n, _, _) = Event.pre_status n

    let rml_pre_value (n, _, _) = Event.pre_value n

    let rml_last (n, _, _) = Event.last n

    let rml_default (n, _, _) = Event.default n

(* ------------------------------------------------------------------------ *)
    let rml_global_signal = new_evt

    let rml_global_signal_combine = new_evt_combine

    let rml_global_signal_memory_combine = new_evt_memory_combine

(* ------------------------------------------------------------------------ *)
(**************************************)
(* configurations                     *)
(**************************************)
    let cfg_present' (n,wa,wp) =
      fun is_long_wait ->
	(fun () -> Event.status n),
        (fun () -> Event.value n),
	[ if is_long_wait then wa else wp ]

    let cfg_present evt_expr =
      fun is_long_wait ->
	let evt = evt_expr() in
	cfg_present' evt is_long_wait

    let cfg_and c1 c2 =
      fun is_long_wait ->
	let is_true1, get1, evt_list1 = c1 is_long_wait in
	let is_true2, get2, evt_list2 = c2 is_long_wait in
	(fun () -> is_true1() && is_true2()),
        (fun () -> get1(), get2()),
	rev_app evt_list1 evt_list2

    let _cfg_or c1 c2 =
      fun is_long_wait ->
	let is_true1, get1, evt_list1 = c1 is_long_wait in
	let is_true2, get2, evt_list2 = c2 is_long_wait in
	(fun () -> is_true1() || is_true2()),
        (fun () -> if is_true1() then get1() else get2()),
	rev_app evt_list1 evt_list2

    let cfg_or_option c1 c2 =
      fun is_long_wait ->
        let is_true1, get1, evt_list1 = c1 is_long_wait in
        let is_true2, get2, evt_list2 = c2 is_long_wait in
        (fun () -> is_true1() || is_true2()),
        (fun () ->
          (if is_true1() then Some (get1()) else None),
          (if is_true2() then Some (get2()) else None)),
        rev_app evt_list1 evt_list2



(* ------------------------------------------------------------------------ *)

(**************************************)
(* nothing                            *)
(**************************************)
    let rml_nothing =
      fun f_k _ctrl ->
	let f_nothing =
	  fun _ ->
	    f_k unit_value
	in f_nothing

(**************************************)
(* compute                            *)
(**************************************)
    let rml_compute e =
      fun f_k _ctrl ->
	let f_compute =
	  fun _ ->
	    let v = e() in
	    f_k v
	in f_compute

(**************************************)
(* pause                              *)
(**************************************)
    let rml_pause =
      fun f_k ctrl ->
	let f_pause =
	  fun _ ->
			assert (C.send_poll ctrl.next f_k)
	in f_pause

(**************************************)
(* pause_kboi                         *)
(**************************************)
    let rml_pause_kboi =
      fun _f_k _ctrl ->
	fun _ -> raise RML

(**************************************)
(* halt                               *)
(**************************************)
    let rml_halt =
      fun _f_k _ctrl ->
	let f_halt =
	  fun _ ->
	    ()
	in f_halt

(**************************************)
(* halt_kboi                          *)
(**************************************)
    let rml_halt_kboi = rml_halt

(**************************************)
(* emit                               *)
(**************************************)
    let step_emit f_k _ctrl (n,wa,wp) e _ =
			Event.lock ();
      Event.emit n (e());
      wakeUp true wa;
      wakeUp true wp;
			Event.unlock ();
      f_k unit_value

    let rml_emit_val expr_evt e =
      fun f_k ctrl ->
	let f_emit_val =
	  fun _ ->
	    let evt = expr_evt() in
	    step_emit f_k ctrl evt e unit_value
	in f_emit_val

    let rml_emit_val' evt e =
      fun f_k ctrl ->
	let f_emit_val =
	  step_emit f_k ctrl evt e
	in f_emit_val

    let rml_emit expr_evt = rml_emit_val expr_evt (fun() -> ())
    let rml_emit' evt = rml_emit_val' evt (fun() -> ())

    let rml_expr_emit_val (n,wa,wp) v =
			Event.lock ();
      Event.emit n v;
      wakeUp true wa;
      wakeUp true wp;
			Event.unlock ()

    let rml_expr_emit evt =
      rml_expr_emit_val evt ()

(**************************************)
(* await_immediate                    *)
(**************************************)
    let step_await_immediate f_k ctrl (n,wa,wp) =
      let w = if ctrl.kind = Top then wa else wp in
      if ctrl.kind = Top then
	let rec f_await_top =
	  fun _ ->
			Event.lock ();
	    if Event.status n
	    then
				let () = Event.unlock () in
	      f_k unit_value
	    else
				let () = 
	      assert (C.send_poll w f_await_top)
				in Event.unlock ()
	in f_await_top
      else
	let rec f_await_not_top =
	  fun _ ->
			Event.lock ();
	    if Event.status n
	    then
				let () = Event.unlock () in
	      f_k unit_value
	    else
				let () = 
	      if !eoi
	      then
		assert (C.send_poll ctrl.next f_await_not_top)
	      else
		(assert (C.send_poll w f_await_not_top);
		 assert (C.send_poll toWakeUp w))
				in Event.unlock ()
	in f_await_not_top

    let rml_await_immediate expr_evt =
      fun f_k ctrl ->
	let f_await =
	  fun _ ->
	    let evt = expr_evt() in
	    step_await_immediate f_k ctrl evt unit_value
	in f_await

    let rml_await_immediate' evt =
      fun f_k ctrl ->
	let f_await =
	  step_await_immediate f_k ctrl evt
	in f_await

(**************************************)
(* await_immediate_conf               *)
(**************************************)
    let rml_await_immediate_conf expr_cfg =
      fun f_k ctrl ->
	if ctrl.kind = Top then
	  let f_await_top =
	    fun _ ->
	      let is_true, _, w_list = expr_cfg true in
				Event.lock ();
	      if is_true() then
		begin
			Event.unlock ();
			f_k unit_value
		end
	      else
		let ref_f = ref None in
		let f w step_wake_up =
			Event.lock ();
		  if is_true() then
		    (ref_f := None;
				 Event.unlock ();
		     f_k unit_value)
		  else
				begin
					assert (C.send_poll w step_wake_up);
					Event.unlock ();
				end
		in
		let gen_step w =
		  let rec step_wake_up _ =
		    match !ref_f with
		    | None -> ()
		    | Some f -> f w step_wake_up
		  in step_wake_up
		in
		begin
			ref_f := Some f;
			List.iter
				(fun w -> assert (C.send_poll w (gen_step w)))
				w_list;
				Event.unlock ()
		end
	  in f_await_top
	else
	  let f_await_not_top =
	    fun _ ->
	      let is_true, _, w_list = expr_cfg false in
				Event.lock ();
	      if is_true() then
		begin
			Event.unlock ();
			f_k unit_value
		end
	      else
		let ref_f = ref None in
		let f w step_wake_up =
			Event.lock ();
		  if is_true() then
		    (ref_f := None;
				 Event.unlock ();
		     f_k unit_value)
		  else
				begin
					if !eoi
					then
						assert (C.send_poll ctrl.next step_wake_up)
					else
						(assert (C.send_poll w step_wake_up);
						assert (C.send_poll toWakeUp w));
					Event.unlock ();
				end
		in
		let gen_step w =
		  let rec step_wake_up _ =
		    match !ref_f with
		    | None -> ()
		    | Some f -> f w step_wake_up
		  in step_wake_up
		in
		begin
			ref_f := Some f;
			List.iter
				(fun w ->
					assert (C.send_poll w (gen_step w));
					assert (C.send_poll toWakeUp w))
				w_list;
			Event.unlock ();
		end 
	  in f_await_not_top

(**************************************)
(* get                                *)
(**************************************)
    let step_get f_k ctrl (n,_,_) p =
      let rec f_get =
	fun _ ->
		Event.lock ();
	  if !eoi
	  then
	    let x =
	      if Event.status n
	      then Event.value n
	      else Event.default n
	    in
	    let f_body = p x f_k ctrl in
			assert (C.send_poll ctrl.next f_body)
	  else
			assert (C.send_poll weoi f_get);
		Event.unlock ();
      in f_get

    let rml_get expr_evt p =
      fun f_k ctrl ->
	let f_get =
	  fun _ ->
	    let evt = expr_evt() in
	    step_get f_k ctrl evt p unit_value
	in f_get

    let rml_get' evt p =
      fun f_k ctrl ->
	let f_get =
	    step_get f_k ctrl evt p
	in f_get




(**************************************)
(* await_immediate_one                *)
(**************************************)
    let step_await_immediate_one f_k ctrl (n,wa,wp) p =
      let w = if ctrl.kind = Top then wa else wp in
      let f_await_one =
	if ctrl.kind = Top then
	  let rec f_await_one_top =
	    fun _ ->
				Event.lock ();
	      if Event.status n
	      then
		let x = Event.one n in
		let () = Event.unlock () in
		p x f_k ctrl unit_value
	      else
		(assert (C.send_poll w f_await_one_top);
		 Event.unlock ())
	  in f_await_one_top
	else
	  let rec f_await_one_not_top =
	    fun _ ->
				Event.lock ();
	      if Event.status n
	      then
		let x = Event.one n in
		let () = Event.unlock () in
		p x f_k ctrl unit_value
	      else
					begin
		if !eoi
		then
		  assert (C.send_poll ctrl.next f_await_one_not_top)
		else
		  (assert (C.send_poll w f_await_one_not_top);
			 assert (C.send_poll toWakeUp w));
		Event.unlock ();
					end 
	  in f_await_one_not_top
      in f_await_one

     let rml_await_immediate_one expr_evt p =
      fun f_k ctrl ->
      let f_await_one =
	fun _ ->
	  let evt = expr_evt() in
	  step_await_immediate_one f_k ctrl evt p unit_value
      in f_await_one

    let rml_await_immediate_one' evt p =
      fun f_k ctrl ->
 	step_await_immediate_one f_k ctrl evt p


(**************************************)
(* await_all_match                    *)
(**************************************)
    let step_await_all_match f_k ctrl (n,wa,wp) matching p = assert false
      (* let w = if ctrl.kind = Top then wa else wp in
      let f_await_all_match =
	if ctrl.kind = Top then
	  let rec f_await_top =
	    fun _ ->
	      if !eoi
	      then
		let v = Event.value n in
		if Event.status n && matching v
		then
		  let x = v in
		  let f_body = p x f_k ctrl in
		  ctrl.next <- f_body :: ctrl.next;
		  sched()
		else
		  (w := f_await_top :: !w;
		   sched ())
	      else
		if Event.status n
		then
		  (weoi := f_await_top :: !weoi;
		   sched ())
		else
		  (w := f_await_top :: !w;
		   sched ())
	  in f_await_top
	else
	  let rec f_await_not_top =
	    fun _ ->
	      if !eoi
	      then
		let v = Event.value n in
		if Event.status n && matching v
		then
		  let x = v in
		  let f_body = p x f_k ctrl in
		  ctrl.next <- f_body :: ctrl.next;
		  sched()
		else
		    (ctrl.next <- f_await_not_top :: ctrl.next;
		     sched ())
	      else
		(w := f_await_not_top :: !w;
		 toWakeUp := w :: !toWakeUp;
		 sched ())
	  in f_await_not_top
      in f_await_all_match *)


    let rml_await_all_match expr_evt matching p =
      fun f_k ctrl ->
	let f_await_all_match =
	  fun _ ->
	    let evt = expr_evt() in
	    step_await_all_match f_k ctrl evt matching p unit_value
	in f_await_all_match

    let rml_await_all_match' evt matching p =
      fun f_k ctrl ->
 	step_await_all_match f_k ctrl evt matching p


(**************************************)
(* await_all_match_conf               *)
(**************************************)

    let step_await_all_match_conf f_k ctrl expr_cfg matching p =
      let is_true, get, w_list = expr_cfg (ctrl.kind = Top) in
      let gen_step_wake_up ref_f =
        let rec step_wake_up w _ =
          match !ref_f with
          | None -> ()
          | Some f -> f w step_wake_up
        in step_wake_up
      in
      let f_await_all_match =
	if ctrl.kind = Top then
	  let gen_f_await_top ref_f =
            let f_await_top w step_wake_up =
				Event.lock ();
	      (if !eoi
	      then
		let v = get () in
		if is_true () && matching v
		then
                  (ref_f := None;
		   let x = v in
		   let f_body = p x f_k ctrl in
			 assert (C.send_poll ctrl.next f_body))
		else
		  assert (C.send_poll w (step_wake_up w))
	      else
		if is_true ()
		then
		  assert (C.send_poll weoi (step_wake_up w))
		else
		  assert (C.send_poll w (step_wake_up w)));
				Event.unlock ()
            in f_await_top
	  in
          fun () ->
            let ref_f = ref None in
            let f_await_top = gen_f_await_top ref_f in
            let step_wake_up = gen_step_wake_up ref_f in
            ref_f := Some f_await_top;
            List.iter
              (fun w ->
								assert (C.send_poll w (step_wake_up w));
								assert (C.send_poll toWakeUp w))
              w_list;
            ();
	else
	  let gen_f_await_not_top ref_f =
	    let f_await_not_top w step_wake_up =
				Event.lock ();
	      if !eoi
	      then
		let v = get () in
		if is_true() && matching v
		then
                  (ref_f := None;
		   let x = v in
		   let f_body = p x f_k ctrl in
			 assert (C.send_poll ctrl.next f_body))
		else
		    assert (C.send_poll ctrl.next (step_wake_up w))
	      else
		(assert (C.send_poll w (step_wake_up w));
			assert (C.send_poll toWakeUp w));
				Event.unlock ()
            in f_await_not_top
          in
          fun () ->
            let ref_f = ref None in
            let f_await_not_top = gen_f_await_not_top ref_f in
            let step_wake_up = gen_step_wake_up ref_f in
            ref_f := Some f_await_not_top;
            List.iter
              (fun w ->
								assert (C.send_poll w (step_wake_up w));
								assert (C.send_poll toWakeUp w))
              w_list;
      in f_await_all_match

    let rml_await_all_match_conf expr_cfg matching p =
      fun f_k ctrl ->
 	step_await_all_match_conf f_k ctrl expr_cfg matching p


(**************************************)
(* present                            *)
(**************************************)

    let step_present _f_k ctrl (n,_,wp) f_1 f_2 =
      let rec f_present =
	fun _ ->
		Event.lock ();
	  if Event.status n
	  then
			begin
				Event.unlock ();
		    f_1 unit_value
			end
	  else
			begin 
				if !eoi
				then
					assert (C.send_poll ctrl.next f_2)
				else
					(assert (C.send_poll wp f_present);
					assert (C.send_poll toWakeUp wp));
				Event.unlock ()
			end
      in f_present

    let rml_present expr_evt p_1 p_2 =
      fun f_k ctrl ->
	let f_1 = p_1 f_k ctrl in
	let f_2 = p_2 f_k ctrl in
	let evt = expr_evt () in
	step_present f_k ctrl evt f_1 f_2

    let rml_present' evt p_1 p_2 =
      fun f_k ctrl ->
	let f_1 = p_1 f_k ctrl in
	let f_2 = p_2 f_k ctrl in
	step_present f_k ctrl evt f_1 f_2

(**************************************)
(* present_conf                       *)
(**************************************)
    let rml_present_conf expr_cfg p_1 p_2 =
      fun f_k ctrl ->
	fun _ ->
	  let f_1 = p_1 f_k ctrl in
	  let f_2 = p_2 f_k ctrl in
	  let is_true, _, w_list = expr_cfg false in
		Event.lock ();
	  if is_true ()
	  then
			let () = Event.unlock () in
	    f_1 unit_value
	  else begin
	    let ref_f = ref None in
	    let f w step_wake_up =
				Event.lock ();
	      if is_true() then
		(ref_f := None;
		 Event.unlock ();
		 f_1 unit_value)
	      else begin
		if !eoi
		then
			assert (C.send_poll ctrl.next f_2)
		else
			(assert (C.send_poll w step_wake_up);
			 assert (C.send_poll toWakeUp w));
		Event.unlock ()
				end 
	    in
	    let gen_step w =
	      let rec step_wake_up _ =
	      match !ref_f with
	      | None -> ()
	      | Some f -> f w step_wake_up
	      in step_wake_up
	    in
	    ref_f := Some f;
	    List.iter
	      (fun w ->
					assert (C.send_poll w (gen_step w));
					assert (C.send_poll toWakeUp w))
	      w_list;
			Event.unlock ()
		end

(**************************************)
(* seq                                *)
(**************************************)

    let rml_seq p_1 p_2 =
      fun f_k ctrl ->
	let f_2 = p_2 f_k ctrl in
	let f_1 = p_1 (fun _x -> f_2 ()) ctrl in
	f_1

(**************************************)
(* par                                *)
(**************************************)
(* Utilisation de Obj.magic pour le pb de la generalisation des *)
(* applications partielles.                                     *)

    let join cpt =
      fun f_k _ctrl ->
	let f_join =
	  fun _ ->
			Mutex.lock (snd cpt);
	    incr (fst cpt);
	    if !(fst cpt) = 2
	    then (
	      (* cpt := 0; *)
				Mutex.unlock (snd cpt);
	      f_k unit_value
	     )
	    else Mutex.unlock (snd cpt);
	in f_join

    let rml_par p_1 p_2 =
      fun f_k ctrl ->
	let cpt = newRef 0 in
	let j = join cpt f_k ctrl in
	let f_1 = p_1 (Obj.magic j: 'a step) ctrl in
	let f_2 = p_2 (Obj.magic j: 'b step) ctrl in
	let f_par =
	  fun _ ->
	    fst cpt := 0;
			assert (C.send_poll current (T.async pool f_2));
	    f_1 unit_value
	in f_par

(**************************************)
(* merge                              *)
(**************************************)

    let rml_merge _p_1 _p_2 =
      fun _f_k _ctrl ->
	fun _ -> raise RML


(**************************************)
(* loop                               *)
(**************************************)
(*
let rec rml_loop p f_k ctrl _ =
  p (rml_loop p f_k ctrl) ctrl unit_value
*)

(*
let rml_loop p =
  fun f_k ctrl ->
    let rec f_1 = lazy (p f ctrl)
    and f =
      fun _ ->
	Lazy.force f_1 unit_value
    in
    f
*)

    let rml_loop p =
      fun _f_k ctrl ->
	let f_1 = ref dummy_step in
	let f_loop = p (fun _ -> !f_1 unit_value) ctrl in
	f_1 := f_loop;
	f_loop

(**************************************)
(* loop_n                             *)
(**************************************)

    let rml_loop_n e p =
      fun f_k ctrl ->
	let cpt = ref 0 in
	let f_1 = ref dummy_step in
	let f_loop =
	  p
	    (fun _ ->
	      if !cpt > 0 then
		(decr cpt; !f_1 unit_value)
	      else
		f_k unit_value)
	    ctrl
	in
	f_1 := f_loop;
	fun _ ->
	  let n = e() in
	  if n > 0 then
	    (cpt := n - 1;
	     f_loop unit_value)
	  else
	    f_k unit_value


(**************************************)
(* signal                             *)
(**************************************)

    let rml_signal p =
      fun f_k ctrl ->
	let f_signal =
	  fun _ ->
	    let evt = new_evt() in
	    let f = p evt f_k ctrl in
	    f unit_value
	in f_signal

    let rml_signal_combine default comb p =
      fun f_k ctrl ->
	let f_signal =
	  fun _ ->
	    let evt = new_evt_combine (default()) (comb()) in
	    let f = p evt f_k ctrl in
	    f unit_value
	in f_signal

    let rml_signal_memory_combine default comb p =
      fun f_k ctrl ->
        let f_signal =
          fun _ ->
            let evt = new_evt_memory_combine (default()) (comb()) in
            let f = p evt f_k ctrl in
            f unit_value
        in f_signal

(**************************************)
(* def                                *)
(**************************************)

    let rml_def e p =
      fun f_k ctrl ->
	let f_def =
	  fun _ ->
	    let f = p (e()) f_k ctrl in
	    f unit_value
	in f_def

(**************************************)
(* def_dyn                            *)
(**************************************)

    let rml_def_dyn p1 p2 =
      fun f_k ctrl ->
	let f_def =
	  p1
	    (fun v ->
	      let f = p2 v f_k ctrl in
	      f unit_value)
	    ctrl
	in f_def

(**************************************)
(* def_and_dyn                        *)
(**************************************)

    (* let rml_def_and_dyn =
      let join_n cpt value_array p3 i =
	fun f_k ctrl ->
	  fun x ->
	    value_array.(i) <- x;
	    decr cpt;
	    if !cpt = 0 then
	      let f = p3 value_array f_k ctrl in
	      f unit_value
	    else
	      sched()
      in
      fun p_array p3 ->
	fun f_k ctrl ->
	  let n = Array.length p_array in
	  let cpt = ref n in
	  let value_array = Array.make n (Obj.magic()) in
	  let step_init =
	    fun _ ->
	      cpt := n;
	      for i = 0 to n - 1 do
		let f =
		  p_array.(i) (join_n cpt value_array p3 i f_k ctrl) ctrl
		in
		current := f :: !current
	      done;
	      sched()
	  in step_init *)

(**************************************)
(* match                              *)
(**************************************)

    let rml_match e p =
      fun f_k ctrl ->
	let f_match =
	  fun _ ->
	    let f = p (e()) f_k ctrl in
	    f unit_value
	in f_match


(**************************************)
(* run                                *)
(**************************************)

    let rml_run e =
      fun f_k ctrl ->
	let f_run =
	  fun _ ->
	    let f_1 = (e ()) () f_k ctrl in
	    f_1 unit_value
	in f_run


(**************************************)
(* until                              *)
(**************************************)
(* ---------- Misc functions for until, control and when ---------- *)
    let new_ctrl kind =
      { kind = kind;
	alive = true;
	susp = false;
	children = [];
	cond = (fun () -> false);
	next = C.make_unbounded ();
	mutex = Mutex.create () }

    let start_ctrl _f_k ctrl f new_ctrl =
      let f_ctrl =
	fun _ ->
		Mutex.lock ctrl.mutex;
	  if new_ctrl.alive
	  then
	    (ctrl.children <- new_ctrl :: ctrl.children)
	  else
	    (new_ctrl.alive <- true;
	     new_ctrl.susp <- false;
	     empty_chan new_ctrl.next (fun _ -> ()));
		Mutex.unlock ctrl.mutex;
	  f unit_value
      in f_ctrl

    let end_ctrl f_k new_ctrl =
      fun x ->
	set_kill new_ctrl;
	Mutex.lock new_ctrl.mutex;
	new_ctrl.alive <- false;
	Mutex.unlock new_ctrl.mutex;
	f_k x
(* ---------------------------------------------------------------- *)

    let rml_until expr_evt p =
      fun f_k ctrl ->
	let new_ctrl = new_ctrl (Kill f_k) in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	let f_until =
	  fun _ ->
	    let (n,_,_) = expr_evt () in
	    new_ctrl.cond <- (fun () -> Event.status n);
	    start_ctrl f_k ctrl f new_ctrl unit_value
	in f_until

    let rml_until' (n,_,_) p =
      fun f_k ctrl ->
	let new_ctrl = new_ctrl (Kill f_k) in
	new_ctrl.cond <- (fun () -> Event.status n);
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	start_ctrl f_k ctrl f new_ctrl

(**************************************)
(* until_conf                         *)
(**************************************)

    let rml_until_conf expr_cfg p =
      fun f_k ctrl ->
	let new_ctrl = new_ctrl (Kill f_k) in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	let f_until =
	  fun _ ->
	    let cond, _, _ = expr_cfg true in
	    new_ctrl.cond <- cond;
	    start_ctrl f_k ctrl f new_ctrl unit_value
	in f_until



(**************************************)
(* until handler                      *)
(**************************************)

    let rml_until_handler_local
	(expr_evt: unit -> ('a, 'b) event) matching_opt p p_handler = assert false
      (* fun f_k ctrl ->
	let evt = ref (Obj.magic() : ('a, 'b) event) in
	let handler =
	  fun () ->
	    let x =
	      let n, _, _ = !evt in
	      if Event.status n
	      then Event.value n
	      else raise RML
	    in
	    let f_handler = p_handler x f_k ctrl in
	    f_handler
	in
	let new_ctrl = new_ctrl (Kill_handler handler) in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	let f_until =
	  fun _ ->
	    let (n, _, _) as e = expr_evt () in
	    evt := e;
	    begin match matching_opt with
	    | None ->
		new_ctrl.cond <- (fun () -> Event.status n);
	    | Some matching ->
		new_ctrl.cond <-
		  (fun () -> Event.status n && matching (Event.value n));
	    end;
	    start_ctrl f_k ctrl f new_ctrl unit_value
	in f_until *)

    let rml_until_handler_local' (n,_,_) matching_opt p p_handler = assert false
      (* fun f_k ctrl ->
	let handler =
	  fun () ->
	    let x =
	      if Event.status n
	      then Event.value n
	      else raise RML
	    in
	    let f_handler = p_handler x f_k ctrl in
	    f_handler
	in
	let new_ctrl = new_ctrl (Kill_handler handler) in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	begin match matching_opt with
	| None ->
	    new_ctrl.cond <- (fun () -> Event.status n);
	| Some matching ->
	    new_ctrl.cond <-
	      (fun () -> Event.status n && matching (Event.value n));
	end;
	start_ctrl f_k ctrl f new_ctrl *)

    let rml_until_handler_conf_local expr_cfg matching_opt p p_handler = assert false
      (* fun f_k ctrl ->
        let ref_get = ref (fun () -> raise RML) in
        let handler =
          fun () ->
            let x = (!ref_get) () in
            let f_handler = p_handler x f_k ctrl in
            f_handler
        in
        let new_ctrl = new_ctrl (Kill_handler handler) in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl in
        let f_until =
          fun _ ->
            let (is_true, get, _) = expr_cfg true in
            ref_get := get;
            begin match matching_opt with
            | None ->
                new_ctrl.cond <- is_true;
            | Some matching ->
                new_ctrl.cond <-
                  (fun () -> is_true () && matching (get ()));
            end;
            start_ctrl f_k ctrl f new_ctrl unit_value
        in f_until *)


    let rml_until_handler expr_evt p p_handler =
      rml_until_handler_local expr_evt None p p_handler

    let rml_until_handler' evt p p_handler =
      rml_until_handler_local' evt None p p_handler

    let rml_until_handler_match expr_evt matching p p_handler =
      rml_until_handler_local expr_evt (Some matching) p p_handler

    let rml_until_handler_match' evt matching p p_handler =
      rml_until_handler_local' evt (Some matching) p p_handler

    let rml_until_handler_conf expr_cfg p p_handler =
      rml_until_handler_conf_local expr_cfg None p p_handler

    let rml_until_handler_match_conf expr_cfg matching p p_handler =
      rml_until_handler_conf_local expr_cfg (Some matching) p p_handler

    let rml_until_match expr_evt matching p =
      rml_until_handler_local expr_evt (Some matching) p (fun _ -> rml_nothing)

    let rml_until_match' evt matching p =
      rml_until_handler_local' evt (Some matching) p (fun _ -> rml_nothing)

    let rml_until_match_conf expr_cfg matching p =
      rml_until_handler_conf_local expr_cfg (Some matching) p (fun _ -> rml_nothing)


(**************************************)
(* control                            *)
(**************************************)
    let rml_control expr_evt p =
      fun f_k ctrl ->
	let new_ctrl = new_ctrl Susp in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	let f_control =
	  fun _ ->
	    let (n, _, _) = expr_evt () in
			Mutex.lock new_ctrl.mutex;
	    new_ctrl.cond <- (fun () -> Event.status n);
			Mutex.unlock new_ctrl.mutex;
	    start_ctrl f_k ctrl f new_ctrl ()
	in f_control

    let rml_control' (n, _, _) p =
      fun f_k ctrl ->
	let new_ctrl = new_ctrl Susp in
	new_ctrl.cond <- (fun () -> Event.status n);
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	start_ctrl f_k ctrl f new_ctrl

(**************************************)
(* control_match                      *)
(**************************************)
    let rml_control_match expr_evt matching p =
      fun f_k ctrl ->
	let new_ctrl = new_ctrl Susp in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	let f_control =
	  fun _ ->
	    let (n, _, _) = expr_evt () in
			Mutex.lock new_ctrl.mutex;
	    new_ctrl.cond <-
	      (fun () -> Event.status n && matching (Event.value n));
			Mutex.unlock new_ctrl.mutex;
	    start_ctrl f_k ctrl f new_ctrl ()
	in f_control

    let rml_control_match' (n, _, _) matching p =
      fun f_k ctrl ->
	let new_ctrl = new_ctrl Susp in
	new_ctrl.cond <- (fun () -> Event.status n && matching (Event.value n));
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	start_ctrl f_k ctrl f new_ctrl

    let rml_control_match_conf expr_cfg matching p =
      fun f_k ctrl ->
        let new_ctrl = new_ctrl Susp in
        let f = p (end_ctrl f_k new_ctrl) new_ctrl in
        let f_control =
          fun _ ->
            let cond, get, _ = expr_cfg true in
						Mutex.lock new_ctrl.mutex;
            new_ctrl.cond <- (fun () -> cond () && matching (get ()));
						Mutex.unlock new_ctrl.mutex;
            start_ctrl f_k ctrl f new_ctrl ()
        in f_control

(**************************************)
(* control_conf                       *)
(**************************************)

    let rml_control_conf expr_cfg p =
      fun f_k ctrl ->
	let new_ctrl = new_ctrl Susp in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	let f_control =
	  fun _ ->
	    let cond, _, _ = expr_cfg true in
	    new_ctrl.cond <- cond;
	    start_ctrl f_k ctrl f new_ctrl ()
	in f_control



(**************************************)
(* when                               *)
(**************************************)

    let step_when _f_k ctrl (n,wa,wp) f new_ctrl dummy =
      let w = if ctrl.kind = Top then wa else wp in
      (Mutex.lock new_ctrl.mutex;
			 new_ctrl.cond <- (fun () -> Event.status n);
			 Mutex.unlock new_ctrl.mutex);
      let rec f_when =
	fun _ ->
		Event.lock ();
	  if Event.status n
	  then
	    (Mutex.lock new_ctrl.mutex;
			 new_ctrl.susp <- false;
			 Mutex.unlock new_ctrl.mutex;
	     next_to_current true new_ctrl)
	  else
	    if !eoi
	    then
				assert (C.send_poll ctrl.next f_when)
	    else
				(assert (C.send_poll w f_when);
	       if ctrl.kind <> Top then assert (C.send_poll toWakeUp w));
			Event.unlock ()
      in
      let start_when =
	fun _ ->
	  if new_ctrl.alive
	  then
	    (Mutex.lock ctrl.mutex;
			 ctrl.children <- new_ctrl :: ctrl.children;
			 Mutex.unlock ctrl.mutex)
	  else
	    (Mutex.lock new_ctrl.mutex;
			 new_ctrl.alive <- true;
	     new_ctrl.susp <- false;
	     empty_chan new_ctrl.next (fun _ -> ());
			 Mutex.unlock new_ctrl.mutex);
		Event.lock ();
	  if Event.status n
	  then
	    (Mutex.lock new_ctrl.mutex;
			 new_ctrl.susp <- false;
	     empty_chan new_ctrl.next (fun _ -> ());
			 Mutex.unlock new_ctrl.mutex;
			 Event.unlock ();
	     f unit_value)
	  else
	    (Mutex.lock new_ctrl.mutex;
			 new_ctrl.susp <- true;
			 empty_chan new_ctrl.next (fun _ -> ());
			 assert (C.send_poll new_ctrl.next f);
			 Mutex.unlock new_ctrl.mutex;
			 assert (C.send_poll w f_when);
	     if ctrl.kind <> Top then assert (C.send_poll toWakeUp w);
			 Event.unlock ())
      in
      dummy := f_when;
      start_when

      let rml_when expr_evt p =
      fun f_k ctrl ->
	let dummy = ref dummy_step in
	let new_ctrl = new_ctrl (When dummy) in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	let start_when =
	  fun _ ->
	    let evt = expr_evt () in
	    step_when f_k ctrl evt f new_ctrl dummy unit_value
	in
	start_when

    let rml_when' evt p =
      fun f_k ctrl ->
	let dummy = ref dummy_step in
	let new_ctrl = new_ctrl (When dummy) in
	let f = p (end_ctrl f_k new_ctrl) new_ctrl in
	let start_when =
	  step_when f_k ctrl evt f new_ctrl dummy
	in
	start_when
	

(**************************************)
(* when_conf                          *)
(**************************************)
    let rml_when_conf _expr_cfg =
      fun _f_k _ctrl ->
	fun _ -> raise RML


(**************************************)
(* if                                 *)
(**************************************)

    let rml_if e p_1 p_2 =
      fun f_k ctrl ->
	let f_1 = p_1 f_k ctrl in
	let f_2 = p_2 f_k ctrl in
	let f_if =
	  fun _ ->
	    if e() then
	      f_1 unit_value
	    else
	      f_2 unit_value
	in f_if


(**************************************)
(* while                              *)
(**************************************)

    let rml_while e p =
      fun f_k ctrl ->
	let f_body = ref dummy_step in
	let f_while =
	  fun _ ->
	    if e()
	    then !f_body unit_value
	    else f_k unit_value
	in
	f_body := p f_while ctrl;
	f_while


(**************************************)
(* for                                *)
(**************************************)

    let rml_for e1 e2 dir p =
      let (incr, cmp) = if dir then incr, (<=) else decr, (>=) in
      fun f_k ctrl ->
	let rec f_for i v2 =
	  fun _ ->
	    incr i;
	    if cmp !i v2
	    then p !i (f_for i v2) ctrl unit_value
	    else f_k unit_value
	in
	let f_for_init =
	  fun _ ->
	    let i = ref (e1()) in
	    let v2 = e2() in
	    if cmp !i v2
	    then p !i (f_for i v2) ctrl unit_value
	    else f_k unit_value
	in
	f_for_init


(**************************************)
(* for_dopar                          *)
(**************************************)
    let join_n cpt =
      fun f_k _ctrl ->
	let f_join_n =
	  fun _ ->
			Mutex.lock (snd cpt);
	    decr (fst cpt);
	    if !(fst cpt) = 0 then
				begin
					Mutex.unlock (snd cpt);
		      f_k unit_value
				end
	    else
	      Mutex.unlock (snd cpt)
	in f_join_n

    let rml_fordopar e1 e2 dir p =
      fun f_k ctrl ->
	let cpt = newRef 0 in
	let j = join_n cpt f_k ctrl in
	let f_fordopar =
	  fun _ ->
	    if dir then
	      begin
		let min = e1() in
		let max = e2() in
		fst cpt := max - min + 1;
		if !(fst cpt) <= 0 then
		  f_k unit_value
		else
		  begin
		    for i = max downto min do
		      let f = p i j ctrl in
					assert (C.send_poll current (T.async pool f));
		    done;
		  end
	      end
	    else
	      begin
		let max = e1() in
		let min = e2() in
		fst cpt := max - min + 1;
		if !(fst cpt) <= 0 then
		  f_k unit_value
		else
		  begin
				for i = max downto min do
		      let f = p i j ctrl in
					assert (C.send_poll current (T.async pool f));
		    done;
		  end
	      end
	in
	f_fordopar


(* ------------------------------------------------------------------------ *)
(**************************************)
(* await                              *)
(**************************************)

    let rml_await expr_evt =
      fun f_k ctrl ->
	fun _ ->
	  let evt = expr_evt () in
	  rml_await_immediate' evt (rml_pause f_k ctrl) ctrl unit_value

    let rml_await' evt =
      fun f_k ctrl ->
	rml_await_immediate' evt (rml_pause f_k ctrl) ctrl

    let rml_await_all expr_evt p =
      fun f_k ctrl ->
	fun _ ->
	  let evt = expr_evt () in
	  rml_await_immediate' evt (rml_get' evt p f_k ctrl) ctrl unit_value

    let rml_await_all' evt p =
      fun f_k ctrl ->
	rml_await_immediate' evt (rml_get' evt p f_k ctrl) ctrl

    let step_get_cfg f_k ctrl get p =
      let f_get_cfg_eoi _ =
        let x = get () in
        let f_body = p x f_k ctrl in
				assert (C.send_poll ctrl.next f_body)
      in
      fun _ ->
				assert (C.send_poll weoi f_get_cfg_eoi)

    let rml_await_all_conf expr_cfg p =
      fun f_k ctrl ->
	fun _ ->
	  let (_, get, _) as cfg = expr_cfg true in
	  rml_await_immediate_conf (fun _ -> cfg)
            (step_get_cfg f_k ctrl get p) ctrl unit_value

    let rml_await_one expr_evt p =
      let pause_p x =
	fun f_k ctrl ->
	  rml_pause (p x f_k ctrl) ctrl
      in
      fun f_k ctrl ->
	fun _ ->
	  let evt = expr_evt () in
	  rml_await_immediate_one' evt pause_p f_k ctrl unit_value

    let rml_await_one' evt p =
      let pause_p x =
	fun f_k ctrl ->
	  rml_pause (p x f_k ctrl) ctrl
      in
      fun f_k ctrl ->
	rml_await_immediate_one' evt pause_p f_k ctrl

    let rml_await_one_match expr_evt matching p =
      rml_await_all_match expr_evt
        (fun x -> List.exists matching x)
        (fun l ->
          try
            let v = List.find matching l in
            p v
          with Not_found -> raise RML)

    let rml_await_one_match' evt matching p =
      rml_await_all_match' evt
        (fun x -> List.exists matching x)
        (fun l ->
          try
            let v = List.find matching l in
            p v
          with Not_found -> raise RML)

    let rml_await_conf expr_cfg =
      fun f_k ctrl ->
	rml_await_immediate_conf expr_cfg (rml_pause f_k ctrl) ctrl

(* ------------------------------------------------------------------------ *)
(*
    let join_n cpt =
      fun f_k ctrl ->
	let f_join_n =
	  fun() ->
	    decr cpt;
	    if !cpt = 0 then
	      f_k()
	    else
	      sched()
	in f_join_n
*)
    let rml_par_n p_list =
      fun f_k ctrl ->
	let nb = List.length p_list in
	let cpt = newRef nb in
	let j = join_n cpt f_k ctrl in
	let f_list = List.map (fun p -> p j ctrl) p_list in
	let f_par_n =
	  fun _ ->
	    fst cpt := nb;
			List.iter (fun p -> assert (C.send_poll current (T.async pool p))) f_list;
	in f_par_n

    let _rml_seq_n p_list =
      fun f_k ctrl ->
	let f =
	  List.fold_right (fun p -> fun k -> p k ctrl) p_list f_k
	in f

(* ------------------------------------------------------------------------ *)
    exception End


(**************************************************)
(* rml_make                                       *)
(**************************************************)
    let rml_make p =
     let result = ref None in
      (* the main step function *)
      let f = p () (fun x -> result := Some x; raise End) top in
      assert (C.send_poll to_launch f);
      (* the react function *)
      let rml_react () =
	try
		await ();
	  eoi := true;
	  wakeUp false weoi;
	  wakeUpAll ();
	  await ();
	  eval_control_and_next_to_current ();
	  Event.next ();
	  eoi := false;
	  None
	with
	| End -> !result
      in
      rml_react


(**************************************************)
(* rml_make_unit                                  *)
(**************************************************)

    let rml_make_unit (p: unit process) = assert false
(*
      (* Function to create the last continuation of a toplevel process *)
      let join_end =
	let term_cpt = ref 0 in
	fun () ->
	  incr term_cpt;
	  let f _x =
	    decr term_cpt;
	    if !term_cpt > 0 then
	      sched()
	    else
	      raise End
	  in f
      in

      (* the main step function *)
      let f = p () (join_end()) top in
      current := [f];

      (* the react function *)
      let rml_react () =
	try
	  sched ();
	  eoi := true;
	  wakeUp weoi;
	  wakeUpAll ();
	  sched ();
	  eval_control_and_next_to_current ();
	  Event.next ();
	  eoi := false;
	  None
	with
	| End -> Some ()
      in

      (* the add_process function*)
      let add_process p =
	let f =  p () (join_end()) top in
	current := f :: !current
      in

      rml_react, add_process
*)
(**************************************************)
(* rml_make_exec_process                          *)
(**************************************************)

    let rml_make_exec_process (p: unit process) = assert false
(*
      (* Function to create the last continuation of a toplevel process *)
      let join_end =
	let term_cpt = ref 0 in
	fun () ->
	  incr term_cpt;
	  let f _x =
	    decr term_cpt;
	    if !term_cpt > 0 then
	      ()
	    else
	      raise End
	  in f
      in

      (* the add_process function*)
      let add_process p =
	let f =  p () (join_end()) top in
	current := (T.async pool f) :: !current
      in

      (* the main step function *)
      let f = p () (join_end()) top in
      current := [T.async pool f];

      (* the react function *)
      let rml_react proc_list =
	try
	  List.iter add_process proc_list;
	  await();
	  eoi := true;
	  wakeUp weoi;
	  wakeUpAll ();
		await();
	  eval_control_and_next_to_current ();
	  Event.next ();
	  eoi := false;
	  None
	with
	| End -> Some ()
      in

      rml_react

*)

  end (* Module Rml_interpreter *)