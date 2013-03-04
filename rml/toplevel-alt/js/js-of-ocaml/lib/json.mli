(* Js_of_ocaml
 * http://www.ocsigen.org
 * Copyright Grégoire Henry 2010.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** Unsafe IO. (See {!Deriving_Json} for typesafe IO) *)

(** Marshall any OCaml value into this JSON representation. *)
val output: 'a -> Js.js_string Js.t

(** Unmarshall a string in JSON format as an OCaml value (unsafe but
    fast !). *)
val unsafe_input: Js.js_string Js.t -> 'a

