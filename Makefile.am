AUTOMAKE_OPTIONS = foreign

ACLOCAL_AMFLAGS = -I build-aux

SUBDIRS = doc

lib_LTLIBRARIES = libmixp.la

libmixp_la_SOURCES = gexpat.c
libmixp_la_CFLAGS = @EXPAT_CFLAGS@
libmixp_la_LDFLAGS = -version-info @LIBCURRENT@:@LIBREVISION@:@LIBAGE@ -export-dynamic
libmixp_la_LIBADD = @EXPAT_LIBS@

bin_PROGRAMS = guile-expat
guile_expat_SOURCES = guile-expat.c
guile_expat_CFLAGS = @GUILE_CFLAGS@
guile_expat_LDFLAGS = @GUILE_LDFLAGS@
guile_expat_LDADD = libmixp.la

noinst_HEADERS = generic_handlers.h gexpat.h

EXTRA_DIST = BUGS test xml

BUILT_SOURCES = gexpat.x

subpkgdatadir = @GUILE_SITE@/xml
subpkgdata_DATA = xml/mixp.scm xml/expat.scm

gexpat.x: gexpat.h generic_handlers.h

TESTS = check-mixp

SUFFIXES = .x
## We assume the user has already installed Guile.
SNARF=guile-snarf
.c.x:
	$(SNARF) -o $@ $< $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)

CLEANFILES = *.x
