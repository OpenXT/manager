(*
 * Copyright (c) 2012 Citrix Systems, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open Db_core
open Dbus_client
open Printf

module D = Debug.Debugger(struct let name="dbd" end)
open D

let bus = DBus.Bus.get DBus.Bus.System
let xs = Xenstore.Xs.daemon_open ()

let sender_name msg =
	match DBus.Message.get_sender msg with
		| None -> ""
		| Some sender -> sender

let get_sender_domid msg =
	match DBus.Message.get_sender msg with
	| None -> None
	| Some sender ->
		  let domid = org_freedesktop_DBus_GetConnectionDOMID
			  "org.freedesktop.DBus"
			  "/org/freedesktop/DBus"
			  sender
		  in
		  Some (Int32.to_int domid)

let uuid_of_domid domid =
	let path = xs.Xenstore.Xs.read ( "/local/domain/" ^ string_of_int domid ^ "/vm" ) in
	match path with
	| "" -> None
	| _  ->
		  let uuid = xs.Xenstore.Xs.read ( path ^ "/uuid" ) in
		  match uuid with
		  | "" -> None
		  | _  -> Some uuid

(* convert the domain path to the subpath the domain has access to or fail miserably
   TODO: maybe we should cache sender/uuids to avoid unnecesarry lookups. Have to figure if the
   dbus sender ids are guaranteed to not be reused though *)
let get_domain_path_or_fail msg path_str =
	match get_sender_domid msg with
	| None   -> failwith "failed to get the domid given message sender"
	| Some 0 -> path_str (* dom0 accesses whole tree *)
	| Some domid ->
		  match uuid_of_domid domid with
		  | None -> failwith ("failed to resolve domid " ^ string_of_int domid ^ " to uuid")
		  | Some uuid -> "/dom-store/" ^ uuid ^ "/" ^ path_str

(* Operations exposed over dbus *)
let com_citrix_xenclient_db_read msg p =
	let path = Tree.path_of_string ( get_domain_path_or_fail msg p ) in
	let v = Tree.read !tree path in
	let to_string jsonv =
		if Json.is_scalar jsonv then
			match jsonv with
			| Json.String s -> s (* avoid json encoding *)
			| _ -> Json.to_string jsonv
		else ""
	in
	match v with
	| None -> ""
	| Some jsonv -> to_string jsonv

let com_citrix_xenclient_db_read_binary msg p =
	let path = Tree.path_of_string ( get_domain_path_or_fail msg p ) in
	let v = Tree.get_subtree !tree path in
	match v with
	| Tree.FilePtr filepath ->
		  let ch  = open_in_bin filepath in
		  try (
			  let sz  = in_channel_length ch in
			  let buf = String.create sz in
			  really_input ch buf 0 sz;
			  close_in ch;
			  let rec to_list acc buf i =
				  if i < 0
				  then acc
				  else to_list (String.get buf i :: acc) buf (i-1)
			  in
			  to_list [] buf (String.length buf - 1)
		  ) with ex ->
			  close_in ch;
			  raise (Failure "input error occured")
	| _ -> raise (Failure "not a binary node")

let com_citrix_xenclient_db_dump msg p =
	let path = Tree.path_of_string ( get_domain_path_or_fail msg p ) in
	let t = Tree.get_subtree !tree path in
	let json = Tree.to_json t in
	string_of_json json

let com_citrix_xenclient_db_list msg p =
	let path = Tree.path_of_string ( get_domain_path_or_fail msg p ) in
	let t = Tree.get_subtree !tree path in
	match t with
	| Tree.Nodes ns ->
		  let names = List.map fst ns in
		  names
	| _ -> []

let com_citrix_xenclient_db_exists msg p =
	let path = Tree.path_of_string ( get_domain_path_or_fail msg p ) in
	Tree.exists !tree path

(* updates *)
let com_citrix_xenclient_db_write msg p v =
	info "write %s %s %s" (sender_name msg) p v;
	let path = Tree.path_of_string ( get_domain_path_or_fail msg p ) in
	let value = Json.String v in
	tree := Tree.write !tree path value;
	tree_dirty := true;
	()

let com_citrix_xenclient_db_inject msg p v =
	info "inject %s %s %s" (sender_name msg) p v;
        let path = Tree.path_of_string ( get_domain_path_or_fail msg p ) in
        let json = Json_parse.of_string v in
        let value = Tree.of_json json in
        tree := Tree.inject !tree path value;
	tree_dirty := true;
	()

let com_citrix_xenclient_db_rm msg p =
	info "rm %s %s" (sender_name msg) p;
	let path = Tree.path_of_string ( get_domain_path_or_fail msg p ) in
	tree := Tree.rm !tree path;
	tree_dirty := true;
	()




