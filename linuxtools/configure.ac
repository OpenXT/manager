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

AC_CHECK_HEADERS([xen/argo.h], [], AC_MSG_ERROR([argo module headers missing.]))

AC_ARG_WITH([libargo],
            AC_HELP_STRING([--with-libargo=PATH], [Path to prefix where libargo was installed.]),
            [LIBARGO_PREFIX=$with_libargo], [])

case "x$LIBARGO_PREFIX" in
    x|xno|xyes)
        LIBARGO_INC=""
        LIBARGO_LIB="-largo"
        LIBARGO_LDFLAGS=""
        ;;
    *)
        LIBARGO_INC="-I${LIBARGO_PREFIX}/include"
        LIBARGO_LDFLAGS="-L${LIBARGO_PREFIX}/lib"
        LIBARGO_LIB="-largo"
        ;;
esac

AC_SUBST(LIBARGO_INC)
AC_SUBST(LIBARGO_LIB)

have_libargo=true

ORIG_LDFLAGS="${LDFLAGS}"
ORIG_CPPFLAGS="${CPPFLAGS}"
    LDFLAGS="${LDFLAGS} ${LIBARGO_LDFLAGS}"
    LIBS="${LIBARGO_LIB}"
    CPPFLAGS="${CPPFLAGS} ${LIBARGO_INC}"
    AC_CHECK_HEADERS([libargo.h], [], [have_libargo=false])
    AC_CHECK_FUNC(argo_socket, [], [have_libargo=false])
    AC_CHECK_FUNC(argo_connect, [], [have_libargo=false])
LDFLAGS="${ORIG_LDFLAGS}"
CPPFLAGS="${ORIG_CPPFLAGS}"

if test "x$have_libargo" = "xfalse"; then
    AC_MSG_ERROR([libargo is missing])
fi

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

