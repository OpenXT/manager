#
# Copyright (c) 2011 Citrix Systems, Inc.
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#

include common.make

PACKAGES = stdext,uuid,log,bigarray,camomile,json,jsonrpc,threads,unix,str,dbus,tscommon,xenmmap,xenbus,xenstore
OCAMLOPT = ocamlfind ocamlopt -linkpkg -package $(PACKAGES)
OCAMLC   = ocamlfind ocamlc -linkpkg -package $(PACKAGES)

OCAMLOPTFLAGS += -g -thread -I autogen
OCAMLCFLAGS += -g -thread -I autogen

db-cmd_OBJS = autogen/db_client db_cmd

dbd_OBJS = autogen/dbus_client db_core db_methods autogen/db_server dbd

ALL_OCAML_OBJS = $(dbd_OBJS) $(dbd-cmd_OBJS)

INTF =

OCAML_PROGRAM = dbd db-cmd

PROGRAMS = $(OCAML_PROGRAM)

all: $(INTF) $(PROGRAMS)

bins: $(PROGRAMS)

INSTALL = install

INSTALL_PROGRAMS_BIN = dbd db-cmd db-cat db-ls db-read db-write db-nodes db-exists db-rm

install-programs:
	$(INSTALL) -d $(DESTDIR)/usr/bin
	$(INSTALL) $(INSTALL_PROGRAMS_BIN) $(DESTDIR)/usr/bin

install: install-programs


include Makefile.rules
