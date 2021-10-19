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

open Pervasiveext
open Stringext
open Unixext

module D = Debug.Debugger(struct let name="dbd" end)
open D

(* function composition *)
let compose f g x = f (g x)
let (<.>)         = compose

(* Union of a single list, removing repeated entries *)
let rec unique = function
	| [] -> []
	| x :: xs -> [x] @ (unique (List.filter (fun y->y<>x) xs))

(* STUFF related to database file names and locations *)
let vm_db_dir       = "/config/vms"
let other_db_file   = "/config/db"
let temp_dbfile filename =
	filename ^ ".tmp"
(* chop off the .tmp extension from database file name maybe *)
let dbfile_basename n =
	if Filename.check_suffix n ".tmp"
	then Filename.chop_suffix n ".tmp"
	else n
(* chop off the .db extension from database file name maybe *)
let chop_db_ext n =
	if Filename.check_suffix n ".db"
	then Filename.chop_suffix n ".db"
	else n
let is_dbfile n = Filename.check_suffix n ".db"

type path = string list

(* how do we place database nodes on disk *)
type filename  = string
type dirname   = string
type placement = FillDir of dirname
type placement_rule =
		{ node_path: path;
		  place: placement; }

let rules = [
	{ node_path = ["vm"]; place = FillDir vm_db_dir };
	{ node_path = ["dom-store"]; place = FillDir "/config/dom-store" }
]

(* tree is dirty when disk update is pending *)
let tree_dirty = ref false

(* storage tree *)
module Tree = struct
	type t =
		Empty
	      | Leaf  of Json.t
	      | FilePtr of string
	      | Nodes of (string * t) list

	(* path identifies tree node *)
	type path = string list

        (* convert tree to JSON representation *)
	let rec to_json = function
	      | Empty    -> Json.Null
	      | Leaf s   -> s
	      | FilePtr s -> Json.Null
	      | Nodes ns ->
			let subs = List.map (fun (name,tree) -> name, to_json tree) ns in
			Json.Object (Array.of_list subs)

	(* get tree from JSON representation *)
	let rec of_json json =
	      match json with
	      | Json.Null      -> Empty
	      | Json.Undefined -> Empty
	      | Json.Int    i -> Leaf json
	      | Json.Float  f -> Leaf json
	      | Json.Bool   b -> Leaf json
	      | Json.String s -> Leaf json
	      | Json.Array  a -> Leaf json
	      | Json.Object o ->
			let conv (name,json) = (name, of_json json) in
			Nodes (List.map conv (Array.to_list o))

        (* convert string to path *)
	let path_of_string s =
		let segments = String.split '/' s in
		(* filter empty ones *)
		List.filter (fun s -> s <> "") segments
		
        (* get subtree of given path *)
	let rec get_subtree tree path =
		match path, tree with
		| [], _ -> tree
		| p :: ps, Nodes ns ->
			  (try
				  let (_,t) = List.find (fun (n,t) -> n = p) ns in
				  get_subtree t ps
			  with Not_found -> Empty)
		| _, _ -> Empty

	let rec exists tree path =
	        match path, tree with
		| [], Leaf _        -> true
		| [], _             -> true
		| p :: ps, Nodes ns ->
		    let matching = List.filter (fun (n,t) -> n = p) ns in
		      (match matching with
		      | (_,t) :: _ -> exists t ps
		      | _          -> false
		      )
		| _ -> false

        (* read value from tree *)
	let rec read tree path =
		match path, tree with
		| [], Leaf s        -> Some s
		| [], FilePtr s     -> Some (Json.String ("file:"^s))
		| [], _             -> None
		| p :: ps, Nodes ns ->
			  let matching = List.filter (fun (n,t) -> n = p) ns in
			  (match matching with
			  | (_,t) :: _-> read t ps
			  | _         -> None
                          )
		| _, _ -> None

        (* inject subtree to tree, return modified tree *)
        let rec inject tree path value =
                match path, tree with
		| [], FilePtr _     -> failwith "cannot overwrite binary file nodes"
		| [], _             -> value
		| p :: ps, Empty
		| p :: ps, Leaf _   -> Nodes [(p, inject Empty ps value)]
		| p :: ps, FilePtr _-> failwith "cannot overwrite binary file nodes"
		| p :: ps, Nodes ns ->
			  let node_names = List.map fst ns in
			  if not (List.mem p node_names) then
				  (* no node with such name at this level, add a new subtree *)
				  let e = (p, inject Empty ps value) in
				  Nodes (ns @ [e])
   			  else
 				  (* there's already node with such name, replace it with new subtree *)
				  Nodes (List.map (fun (n,t) ->
					  if n <> p
					  then (n,t)
					  else (n,inject t ps value)) ns
					)
				
        (* write value to tree, return modified tree *)
	let rec write tree path value =
		match path, tree with
		| [], Nodes ns      -> Nodes ns (* do NOT overwrite node if it has children *)
		| [], FilePtr _     -> failwith "cannot overwrite binary file nodes"
		| [], _             -> Leaf value
		| p :: ps, Empty
		| p :: ps, Leaf _   -> Nodes [(p, write Empty ps value)]
		| p :: ps, FilePtr _-> failwith "cannot overwrite binary file nodes"
		| p :: ps, Nodes ns ->
			  let node_names = List.map fst ns in
			  if not (List.mem p node_names) then
				  (* no node with such name at this level, add a new subtree *)
				  let e = (p, write Empty ps value) in
				  Nodes (ns @ [e])
   			  else
 				  (* there's already node with such name, replace it with new subtree *)
				  Nodes (List.map (fun (n,t) ->
					  if n <> p
					  then (n,t)
					  else (n,write t ps value)) ns
					)

        (* remove subtree *)
	let rec rm tree path =
		match path, tree with
		| [], FilePtr _     -> failwith "cannot remove binary file nodes"
		| [], _             -> Empty
		| p :: ps, Empty    -> Empty
		| p :: ps, FilePtr _-> failwith "cannot remove binary file nodes"
		| p :: ps, Leaf _   -> tree
		| p :: ps, Nodes ns ->
			  let ns' = (List.map (fun (n,t) ->
				  if n <> p
				  then (n,t)
				  else (n,rm t ps)) ns
				) in
			  let scrap_empty = List.filter (fun (_,t) -> t <> Empty) in
			  Nodes (scrap_empty ns')

end

(* convert JSON to pretty string *)
let string_of_json t =
	let to_buffer t buf = Json.to_fct_pretty t (fun s -> Buffer.add_string buf s) in
	let buf = Buffer.create 2048 in
	to_buffer t buf;
	Buffer.contents buf

type dir_entry = { entry_path : string;
		   entry_stats : Unix.stats }

(* list of full paths to each file in directory *)
let directory_contents dir =
	try
		let handle = Unix.opendir dir in
		let rec entries () =
			try
				match Unix.readdir handle with
				| "." | ".." -> entries ()
				| e          -> e :: entries ()
			with End_of_file -> []
		in
		let add_stats e =
		  { entry_path = e;
		    entry_stats = Unix.stat e }
		in
		let contents = List.map add_stats (List.map (Filename.concat dir) (entries())) in
		Unix.closedir handle;
		contents
	with _ ->
		error "Problem reading contents of directory %s" dir;
		[]

let dir_files contents = List.map (fun x -> x.entry_path) (List.filter (fun x -> x.entry_stats.Unix.st_kind = Unix.S_REG) contents)
and dir_subdirs contents = List.map (fun x -> x.entry_path) (List.filter( fun x -> x.entry_stats.Unix.st_kind = Unix.S_DIR) contents)
					
(* the global tree storage object *)
let tree = ref Tree.Empty

let rec write_all fd string off n =
	try
		Unixext.really_write fd string off n
	with
	| Unix.Unix_error (Unix.EAGAIN, _, _) -> write_all fd string off n
	| Unix.Unix_error (Unix.EINTR, _, _) -> write_all fd string off n

let write_db_file t filename =
  match t with
    | Tree.FilePtr _ -> ()
    | _ ->
	(* info "Writing database file %s" filename; *)
	let temp_file = temp_dbfile filename in
	let json = Tree.to_json t in
	let str = string_of_json json in
	let bufsize = String.length str in
	let f = Unix.openfile temp_file [Unix.O_CREAT; Unix.O_WRONLY] 0o644 in
	(* write contents to temporary file *)
	let written = try write_all f str 0 bufsize; bufsize with _ -> 0 in
	if written = bufsize
	then (
		(* success *)
		Unixext.fsync f;
		Unix.close f;
		(* might not exist..*)
		(try Unix.unlink filename with _ -> ());
		Unix.link temp_file filename;
		Unix.unlink temp_file
	) else (
		(* oops something went wrong *)
		Unix.close f;
		error "Problems writing %s database file! Skipping write." filename;
	)

let make_dir dir =
	if not (Sys.file_exists dir) then
		Unix.mkdir dir 0o755
	else
		()

(* possibly should optimise this at some point, but writes are not bottleneck now *)
let rec write_db_using_rule ~unlink_files ~rul =
	let subtree = Tree.get_subtree !tree rul.node_path in
	match subtree with
	| Tree.Nodes nodes ->
		  (match rul.place with
		  | FillDir dirname -> fill_dir nodes dirname unlink_files)
	| _ -> ()

and fill_dir nodes dirname unlink_files =
	make_dir dirname;
	let files = placement_in_dir nodes dirname in
	let write (file,node) =
		let node_name, node_tree = node in
		write_db_file node_tree file
	in

	(* write files *)
	List.iter write (List.combine files nodes);

	(* maybe remove some .db files *)
	if unlink_files then (
		let files_there   = List.filter is_dbfile (dir_files (directory_contents dirname)) in
		(* unlink any files in the directory which we didn't write now -> removes nodes which do not exist anymore *)
		let was_written f = List.mem f files in
		let not_wanted    = List.filter (not <.> was_written) files_there in
		let unlink f      =
			info "Unlinking file %s" f;
			Unix.unlink f
		in
		List.iter unlink not_wanted
	)

and placement_in_dir nodes dir =
	let place (node_name, _) =
		let f = Filename.concat dir node_name in
		f ^ ".db"
	in
	List.map place nodes

let write_db ~unlink_files =
	debug "Updating database";
	(* go through placement rules and write using them *)
	List.iter (fun rul -> write_db_using_rule ~unlink_files ~rul) rules;

	(* write out other non rule based config -> strip all nodes which have rules first*)
	let t = List.fold_left
		(fun acc rul -> Tree.rm acc rul.node_path)
		!tree
		rules
	in
	(* and then output *)
	write_db_file t other_db_file;
	tree_dirty := false

(* read a single database file, filename is actually a basename so it does not contain .tmp suffix if any *)
let read_db_file filename =
	info "Reading database file %s" filename;

	(* cope with .tmp files by renaming them to proper .db files, if the proper one does not exist *)
	let temp = temp_dbfile filename in
	if not (Sys.file_exists filename) && (Sys.file_exists temp) then (
		warn "Recovering from database corruption, renaming %s" temp;
		Unix.link filename temp;
		Unix.unlink temp
	);
	try
		let str = ref "" in
		let f = open_in filename in
		(try
			 while true do
				 str := !str ^ (input_line f) ^ "\n"
			 done
		 with _ ->
			 close_in f);
		(* handle empty files by returning empty tree *)
		let empty = (String.strip String.isspace !str) = "" in
		if empty
		then Tree.Empty
		else Tree.of_json (Json_parse.of_string !str)
	with e -> (
		error "Error while parsing JSON database file %s -> file not read!" filename;
		raise e)

let rec read_db_using_rule db rul =
	match rul.place with
	| FillDir dirname ->
		  poll_dir db rul.node_path dirname

and poll_dir db node_path dir =
	let contents = directory_contents dir in
	(* directory can contain both .tmp and .db files, but we actually only want to consider
	   unique base filenames *)
	let files = unique (List.map dbfile_basename (dir_files contents)) in
	let dbfiles = List.filter is_dbfile files in
	let binfiles = List.filter (not <.> is_dbfile) files in
	let names_plus_trees =
		let all = List.map read_db_file dbfiles in
		let node_names = List.map (chop_db_ext <.> Filename.basename) dbfiles in
		List.iter (fun nn -> info "Detected db node %s in separate file" nn) node_names;
		List.combine node_names all
	in
	let merge tree (name,subtree) =
		Tree.inject tree (node_path @ [name]) subtree
	in
	let db = List.fold_left merge db names_plus_trees
	and binnode_names = List.map Filename.basename binfiles
	and binnode_trees = List.map (fun f -> Tree.FilePtr f) binfiles in
	let db = List.fold_left merge db (List.combine binnode_names binnode_trees) in
        (* merge contents of subdirectories into tree too *)
	let subdirs = dir_subdirs contents
	and subdir_pool db dir =
	  let name = Filename.basename dir in
	    poll_dir db (node_path @ [name]) dir
	in
	  List.fold_left subdir_pool db subdirs


(* read whole database *)			
let read_db () =
	info "Reading database";
	(* non-rule config lives elsewhere *)
	let other_db_tree = read_db_file other_db_file in

	(* merge all the trees obtained from parsing rules into one bloody pulp *)
	let t = List.fold_left read_db_using_rule other_db_tree rules in
			
	(* substitute in memory tree *)
	tree := t;
	tree_dirty := false;
