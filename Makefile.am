#
# Copyright (C) 2007, 2008 Loic Dachary <loic@dachary.org>
# Copyright (C)       2008 Bradley M. Kuhn <bkuhn@ebb.org>
# Copyright (C) 2004, 2005, 2006 Mekensleep <licensing@mekensleep.com>
#                                24 rue vieille du temple, 75004 Paris
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
#
# 
ACLOCAL_AMFLAGS = -I config

MAINTAINERCLEANFILES = INSTALL Makefile.in aclocal.m4 configure \
	$(filter-out %.m4,$(wildcard config/[a-z]*)) 

pythondir = ${pyexecdir}
python_PYTHON = \
	pokereval.py

if PYTHON_2_7
py2_6exec_LTLIBRARIES = _pokereval_2_7.la
_pokereval_2_7_la_SOURCES = pypokereval.c
_pokereval_2_7_la_LDFLAGS = -module -no-undefined -version-info 1:0:0
_pokereval_2_7_la_LIBADD = ${PYTHON2_7_LIBS} ${POKER_EVAL_LIBS}
_pokereval_2_7_la_CFLAGS = ${PYTHON2_7_CFLAGS} ${POKER_EVAL_CFLAGS} -DPYTHON_VERSION=\"2_7\" -D'VERSION_NAME(w)=w\#\#2_7'
endif

if PYTHON_2_6
py2_6exec_LTLIBRARIES = _pokereval_2_6.la
_pokereval_2_6_la_SOURCES = pypokereval.c
_pokereval_2_6_la_LDFLAGS = -module -no-undefined -version-info 1:0:0
_pokereval_2_6_la_LIBADD = ${PYTHON2_6_LIBS} ${POKER_EVAL_LIBS}
_pokereval_2_6_la_CFLAGS = ${PYTHON2_6_CFLAGS} ${POKER_EVAL_CFLAGS} -DPYTHON_VERSION=\"2_6\" -D'VERSION_NAME(w)=w\#\#2_6'
endif

if PYTHON_2_5
py2_5exec_LTLIBRARIES = _pokereval_2_5.la
_pokereval_2_5_la_SOURCES = pypokereval.c
_pokereval_2_5_la_LDFLAGS = -module -no-undefined -version-info 1:0:0
_pokereval_2_5_la_LIBADD = ${PYTHON2_5_LIBS} ${POKER_EVAL_LIBS}
_pokereval_2_5_la_CFLAGS = ${PYTHON2_5_CFLAGS} ${POKER_EVAL_CFLAGS} -DPYTHON_VERSION=\"2_5\" -D'VERSION_NAME(w)=w\#\#2_5'
endif

if PYTHON_2_4
py2_4exec_LTLIBRARIES = _pokereval_2_4.la
_pokereval_2_4_la_SOURCES = pypokereval.c
_pokereval_2_4_la_LDFLAGS = -module -no-undefined -version-info 1:0:0
_pokereval_2_4_la_LIBADD = ${PYTHON2_4_LIBS} ${POKER_EVAL_LIBS}
_pokereval_2_4_la_CFLAGS = ${PYTHON2_4_CFLAGS} ${POKER_EVAL_CFLAGS} -DPYTHON_VERSION=\"2_4\" -D'VERSION_NAME(w)=w\#\#2_4'
endif

if PYTHON_2_3
py2_3exec_LTLIBRARIES = _pokereval_2_3.la
_pokereval_2_3_la_SOURCES = pypokereval.c
_pokereval_2_3_la_LDFLAGS = -module -no-undefined -version-info 1:0:0
_pokereval_2_3_la_LIBADD = ${PYTHON2_3_LIBS} ${POKER_EVAL_LIBS}
_pokereval_2_3_la_CFLAGS = ${PYTHON2_3_CFLAGS} ${POKER_EVAL_CFLAGS} -DPYTHON_VERSION=\"2_3\" -D'VERSION_NAME(w)=w\#\#2_3'
endif

pkgconfigdir = ${libdir}/pkgconfig
pkgconfig_DATA = pypoker-eval.pc

EXTRA_DIST = config/python.m4 config/ccpython.m4 \
	test.py \
	bug-1823.py \
	pypoker-eval.vcproj pypoker-eval.sln \
	INSTALL.windows \
	$(wildcard gentoo/dev-games/pypoker-eval/*.ebuild)

TESTS_ENVIRONMENT = ${PYTHON}
TESTS = test.py bug-1823.py
