#
# Copyright (C) 2007, 2008 Loic Dachary <loic@dachary.org>
# Copyright (C) 2008             Bradley M. Kuhn <bkuhn@ebb.org>
# Copyright (C) 2004, 2005, 2006 Mekensleep <licensing@mekensleep.com>
#                                24 rue vieille du temple, 75004 Paris
#       
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.
#
# Authors:
#  Loic Dachary <loic@dachary.org>
#  Bradley M. Kuhn <bkuhn@ebb.org>

# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.54)
AC_INIT(pypoker-eval, 138.0)
AC_CONFIG_AUX_DIR(config)
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE()
AC_CONFIG_SRCDIR([pypokereval.c])

# Checks for programs.
AC_PROG_MAKE_SET
AC_GNU_SOURCE
AC_LANG(C)
AC_PROG_CXX
AC_PROG_LIBTOOL
AC_PROG_RANLIB

AC_CHECK_FUNCS([memset])

AM_MAINTAINER_MODE

# Check for python
#
# GCC specific debug and optimization flags
#
AC_ARG_ENABLE(debug,
    [  --enable-debug          build in debug mode (the default is to build in optimized mode)],
    [enable_debug=yes])

if test "$GCC" = "yes" -a "$ac_test_CFLAGS" != set ; then
  if test "$enable_debug" = "yes" ; then
    CFLAGS="-pipe -g -Wall -Wpointer-arith -Wstrict-prototypes"
  else
    CFLAGS="-pipe -O3 -Wall -Wpointer-arith -Wstrict-prototypes"
  fi
fi

test x$prefix = xNONE && prefix="$ac_default_prefix"
test x$exec_prefix = xNONE && exec_prefix="$prefix"

ALL_CC_PYTHON

# Check for poker-eval
PKG_CHECK_MODULES(POKER_EVAL, poker-eval >= 124.0)

# Generate files
AC_CONFIG_FILES([Makefile
                 pypoker-eval.pc])
AC_OUTPUT
