AC_INIT
AM_INIT_AUTOMAKE(mixp, 0.4, no-define)
AM_CONFIG_HEADER(config.h)

LIBCURRENT=0
LIBREVISION=0
LIBAGE=0

AC_SUBST(LIBCURRENT)
AC_SUBST(LIBREVISION)
AC_SUBST(LIBAGE)

AC_PROG_CC
AC_ISC_POSIX
AC_PROG_LIBTOOL

GUILE_PROGS
GUILE_FLAGS
GUILE_SITE_DIR

AM_WITH_EXPAT

AH_TEMPLATE(GUILE_GC_MALLOC,
            [define guile gc-ed malloc function name (all peoples hate guile..)])

AC_CHECK_DECL(scm_gc_malloc, AC_DEFINE(GUILE_GC_MALLOC, scm_gc_malloc),
                             AC_DEFINE(GUILE_GC_MALLOC, scm_must_malloc),
                             [#include <libguile.h>])

AC_CONFIG_FILES([check-mixp], [chmod +x check-mixp])

AC_OUTPUT(Makefile)
