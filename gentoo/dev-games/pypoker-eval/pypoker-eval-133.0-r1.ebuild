# Copyright 1999-2005 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2
# $Header$

inherit eutils

DESCRIPTION="python binding for poker-eval"
HOMEPAGE="http://gna.org/projects/pokersource"
MY_P="${PN}-${PV}.tar.gz"
SRC_URI="http://download.gna.org/pokersource/sources/${MY_P}"
SLOT="0"
LICENSE="GPL-2.1"
KEYWORDS="x86 amd64"
IUSE=""

DEPEND="dev-games/poker-eval virtual/python"

src_unpack() {
	unpack ${MY_P}
	if ls ${FILESDIR}/${PVR}*.patch 2>/dev/null
		then
		for i in ${FILESDIR}/${PVR}*.patch
		  do
		  epatch $i
		done
	fi
}


src_install () {
	make install DESTDIR=${D} || die "einstall failed"
}
