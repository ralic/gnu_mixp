# Makefile.am
#
# Copyright (C) 2011 Thien-Thi Nguyen
#
# This file is part of Mixp.
#
# Mixp is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 3, or
# (at your option) any later version.
#
# Mixp is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public
# License along with Mixp; see the file COPYING.  If not,
# write to the Free Software Foundation, Inc., 51 Franklin Street,
# Fifth Floor, Boston, MA  02110-1301  USA

info_TEXINFOS = mixp.texi
mixp_TEXINFOS = ref.texi fdl.texi

AM_MAKEINFOHTMLFLAGS = --no-headers --no-split

EXTRA_DIST = ref.texh mixp.html

if MAINTAINER_MODE

bx = $(top_srcdir)/build-aux
gx = $(bx)/guile-baux/gbaux-do

SUFFIXES = .texh

snippets = ../src/all.snippets

.texh.texi:
	$(gx) tsin -f $(snippets) -c utf-8 $<

ref.texi: $(snippets)

endif # MAINTAINER_MODE

# Makefile.am ends here
