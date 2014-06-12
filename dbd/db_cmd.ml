(*
 * Copyright (c) 2013 Citrix Systems, Inc.
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

open DBus
open Stringext
open Db_client
open Printf

(* convert string to path *)
let path_of_string s =
	let segments = String.split '/' s in
	(* filter empty ones *)
	List.filter (fun s -> s <> "") segments

let do_list = com_citrix_xenclient_db_list "com.citrix.xenclient.db" "/"
let do_read = com_citrix_xenclient_db_read "com.citrix.xenclient.db" "/"
let do_write p v =
	let cannot_do () = fprintf stderr "Nope, not doing that.\n"; exit(1) in
	match path_of_string p with
	| [] | ["vm"] -> cannot_do() (* for some reason folks don't like the feature of erasing database when doing write to root node *)
	| _ -> com_citrix_xenclient_db_write "com.citrix.xenclient.db" "/" p v

let do_rm = com_citrix_xenclient_db_rm "com.citrix.xenclient.db" "/"
let do_exists = com_citrix_xenclient_db_exists "com.citrix.xenclient.db" "/"

let do_cat path =
	let data = com_citrix_xenclient_db_read_binary "com.citrix.xenclient.db" "/" path in
	List.iter (output_char stdout) data

let joinpaths a b = a ^ (if String.endswith "/" a then "" else "/") ^ b
let lastseg path =
	let rec aux = function
	      | []      -> ""
	      | x :: [] -> x
	      | x :: xs -> aux xs
	in
	let splits = String.split '/' path in
	aux splits

let do_ls path full =
	let rec describe indent path =
		let inds = String.make indent ' ' in
		let prefix =
			if not full
			then inds ^ lastseg path
			else path
		in
		(match do_list path with
		| [] -> prefix ^ " = \"" ^ do_read path ^ "\"\n"
		| xs -> (prefix ^ " =\n")
			  ^ List.fold_left (
				  fun acc x ->
					  let child_path = joinpaths path x in
					  let child_d = describe (indent+1) child_path in
					  acc ^ child_d
		          ) "" xs
		)
	in
	describe 0 path


let do_nodes path =
	let nodes = do_list path in
	List.fold_left (fun acc s ->
		match acc with
		| "" -> s
		| _  -> acc ^ " " ^ s
		) "" nodes

let usage () =
	print_endline "Usage: db-cmd <cmd> <parameters>"

let _ =
	match Sys.argv with
	| [| _; "ls"   ; "-f"; path  |] -> print_string (do_ls path true)
	| [| _; "ls"   ; "-f"        |] -> print_string (do_ls "/" true)
	| [| _; "ls"   ; path        |] -> print_string (do_ls path false)
	| [| _; "ls"                 |] -> print_string (do_ls "/" false)
	| [| _; "read" ; path        |] -> print_endline (do_read path)
	| [| _; "cat"  ; path        |] -> do_cat path
	| [| _; "write"; path; value |] -> do_write path value
	| [| _; "nodes"; path        |] -> print_endline (do_nodes path)
	| [| _; "exists"; path       |] -> print_endline (string_of_bool (do_exists path))
	| [| _; "rm"; path           |] -> if path_of_string path <> [] then do_rm path else print_endline "bad path"
	| _                             -> usage (); exit(1)
	
