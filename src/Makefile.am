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

SUFFIXES = .scm

schemestems = utils
schemesource = $(schemestems:=.scm)
EXTRA_DIST = $(schemesource)

bx = $(top_srcdir)/build-aux
gx = $(bx)/guile-baux/gbaux-do
irootdir = $(GUILE_LIBSITE)/mixp

AM_CPPFLAGS = $(GUILE_CFLAGS) $(EXPAT_CFLAGS)
AM_LDFLAGS = -export-dynamic -avoid-version -module -lexpat
if !TRIPPY
AM_CPPFLAGS += -DLAME_LAME_LAME
endif

BUILT_SOURCES = expat.x

snarfcppopts = $(DEFS) $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS)

iroot_DATA = $(schemestems)

if TRIPPY
iroot_LTLIBRARIES = expat.la
else !TRIPPY
pkglib_LTLIBRARIES = expat.la
endif !TRIPPY
expat_la_SOURCES = expat.c gi.h

if !TRIPPY
iroot_DATA += expat

.c.exports:
	> $@ \
	$(SED) '/^(/!d;s/^.*"\(.*\)".*/\1/' $<

expat: expat.la expat.exports
	$(gx) gen-scheme-wrapper \
	  -o $@ $< -g mixp -t init_~A_module \
	  -i '$(pkglibdir)'
endif !TRIPPY

.c.x:
	$(gx) c2x -o $@ $< -- $(snarfcppopts)

.scm:
	$(gx) punify -n $< > $@

if MAINTAINER_MODE

noinst_DATA = all.snippets

all.snippets: c.snippets scheme.snippets
	$(gx) tsar -f $@ -c utf-8 concat $^

c.snippets: $(expat_la_SOURCES)
	$(gx) c-tsar -f $@ -c utf-8 update $^ -- $(snarfcppopts)

scheme.snippets: $(schemesource)
	$(gx) tsar -f $@ -c utf-8 update $^

endif # MAINTAINER_MODE

update-libsite-module-catalog:
	$(mmc) -t '$(DESTDIR)' '$(DESTDIR)$(GUILE_LIBSITE)'

fixargs = '$(DESTDIR)$(irootdir)' $(iroot_LTLIBRARIES)
submake = $(MAKE) $(AM_MAKEFLAGS)

install-data-hook:
if TRIPPY
	-rmdir '$(DESTDIR)$(pkglibdir)'
	$(bx)/sofix no-la,no-symlinks $(fixargs)
	$(submake) update-libsite-module-catalog
else !TRIPPY
	@if [ 1 = '$(FOR_MAKE_CHECK)' ] ; then	\
	  mv -f expat expat.STASH ;		\
	  $(submake) expat ;			\
	  $(INSTALL_DATA) expat $(irootdir) ;	\
	  mv -f expat.STASH expat ;		\
	fi
endif !TRIPPY

uninstall-hook:
if TRIPPY
	$(bx)/uninstall-sofixed $(fixargs)
	$(submake) update-libsite-module-catalog
endif TRIPPY
	-rmdir '$(DESTDIR)$(irootdir)'
	-rmdir '$(DESTDIR)$(pkglibdir)'

## See comment in ../test/Makefile.am.
stamp-all: $(iroot_DATA) $(iroot_LTLIBRARIES) $(pkglib_LTLIBRARIES)
	@touch $@

DISTCLEANFILES = *.x *.exports

CLEANFILES = *.x $(iroot_DATA) stamp-all
if MAINTAINER_MODE
CLEANFILES += *.snippets
endif

# Makefile.am ends here
