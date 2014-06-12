#
# Copyright (c) 2012 Citrix Systems, Inc.
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

AC_INIT([xc-guest-tools], [1.0])
AC_PREREQ([2.59])
AM_INIT_AUTOMAKE([1.10 no-define])

AC_CONFIG_HEADERS([config.h])
AC_PROG_CC

AC_CHECK_HEADERS([xen/v4v.h], [], AC_MSG_ERROR([v4v module headers missing.]))

AC_ARG_WITH([libv4v],
            AC_HELP_STRING([--with-libv4v=PATH], [Path to prefix where libv4v was installed.]),
            [LIBV4V_PREFIX=$with_libv4v], [])

case "x$LIBV4V_PREFIX" in
    x|xno|xyes)
        LIBV4V_INC=""
        LIBV4V_LIB="-lv4v"
        LIBV4V_LDFLAGS=""
        ;;
    *)
        LIBV4V_INC="-I${LIBV4V_PREFIX}/include"
        LIBV4V_LDFLAGS="-L${LIBV4V_PREFIX}/lib"
        LIBV4V_LIB="-lv4v"
        ;;
esac

AC_SUBST(LIBV4V_INC)
AC_SUBST(LIBV4V_LIB)

have_libv4v=true

ORIG_LDFLAGS="${LDFLAGS}"
ORIG_CPPFLAGS="${CPPFLAGS}"
    LDFLAGS="${LDFLAGS} ${LIBV4V_LDFLAGS}"
    LIBS="${LIBV4V_LIB}"
    CPPFLAGS="${CPPFLAGS} ${LIBV4V_INC}"
    AC_CHECK_HEADERS([libv4v.h], [], [have_libv4v=false])
    AC_CHECK_FUNC(v4v_socket, [], [have_libv4v=false])
    AC_CHECK_FUNC(v4v_connect, [], [have_libv4v=false])
LDFLAGS="${ORIG_LDFLAGS}"
CPPFLAGS="${ORIG_CPPFLAGS}"

if test "x$have_libv4v" = "xfalse"; then
    AC_MSG_ERROR([libv4v is missing])
fi

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

