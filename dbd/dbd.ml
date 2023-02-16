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
open Db_core
open Db_server
open Db_methods

module D = Debug.Debugger(struct let name="dbd" end)
open D

let update_interval = 3.0
let default_pidfile = "/var/run/dbd.pid"

let monitor_rpc_dbus pidfile =
	let intf = Printf.sprintf "com.citrix.xenclient.db" in
	(match DBus.Bus.request_name bus intf [DBus.Bus.DoNotQueue] with
	| DBus.Bus.PrimaryOwner -> ()
	| _ -> failwith (Printf.sprintf "cannot grab dbus intf %s" intf)
	);
	db_export_dbus "/" bus;
	Unixext.daemonize ();
	(try Unixext.pidfile_write pidfile with _ -> ());
	let t0 = ref ( Unix.time() ) in
	let dt_without_update = ref 0.0 in
	while true do
		let sleep_time = if !tree_dirty then 3000 else -1 in
		ignore (DBus.Connection.read_write_dispatch bus sleep_time);
		let t1 = Unix.time () in
		let dt = t1 -. !t0 in
		t0 := t1;
		if (dt > 0.0) then (
			dt_without_update := !dt_without_update +. dt
		);
		if !tree_dirty then (
			if (!dt_without_update >= update_interval) then (
				write_db ~unlink_files:true;
				dt_without_update := 0.0
			)
		)
	done

let _ =
	let pidf = ref "" in
	let speclist = [("--pidfile", Arg.Set_string pidf, "");
			("--debug", Arg.Unit (fun() -> debugging := true), "")] in
	let usage_msg = "usage: dbd [--pidfile <filename>] [--debug] [--help]" in
	Arg.parse speclist (fun _ -> ()) usage_msg;

	let pidfile = if !pidf <> "" then !pidf else default_pidfile in

	Logs.set_default Log.Debug ["syslog:dbd"];
	Logs.set_default Log.Info ["syslog:dbd"];
	Logs.set_default Log.Warn ["syslog:dbd"];
	Logs.set_default Log.Error ["syslog:dbd"];
	read_db ();
	(* reread db on sighup *)
	Sys.set_signal Sys.sighup (Sys.Signal_handle (fun _ ->
		info "SIGHUP received: rereading database";
		read_db ()));
	(* make sure to write db on sigterm if dirty *)
	Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ ->
		if !tree_dirty then ( write_db ~unlink_files:true ); exit 0));
	monitor_rpc_dbus pidfile
