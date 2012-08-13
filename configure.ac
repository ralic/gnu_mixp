dnl configure.ac
dnl
dnl Copyright (C) 2011, 2012 Thien-Thi Nguyen
dnl Copyright (C) 2002, 2003 Dmitry Morozhnikov
dnl
dnl This file is part of Mixp.
dnl
dnl Mixp is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 3, or (at your
dnl option) any later version.
dnl
dnl Mixp is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with Mixp; see the file COPYING.  If not, write to the
dnl Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
dnl Boston, MA  02110-1301  USA

AC_PREREQ([2.67])
AC_INIT([Mixp],[0.6],[ttn@gnuvola.org])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_MACRO_DIR([build-aux])
AM_INIT_AUTOMAKE([1.11.1 gnu no-dependencies -Wall])
AM_MAINTAINER_MODE

AM_PROG_AR
AC_PROG_CC
AC_SEARCH_LIBS([strerror],[cposix])
LT_PREREQ([2.2.6])
LT_INIT([dlopen disable-static]) dnl module only

SNUGGLE_PROGS
SNUGGLE_GUILE_LIBSITE_DIR([mixp])
SNUGGLE_GUILE_TOOLS_EXISTSP([mixp_cv_mkmodcat],[make-module-catalog])
AS_IF([test xyes = x$mixp_cv_mkmodcat],
      [mmc='${GUILE_TOOLS} make-module-catalog'],
      [mmc=:])
AC_SUBST([mmc])
AM_CONDITIONAL([TRIPPY],[test x':' != x"$mmc"])

SNUGGLE_FLAGS

dnl Check if --with-expat[=DIR] is specified and
dnl Expat >= 1.95.0 is installed in the system.
dnl If yes, substitute EXPAT_CFLAGS, EXPAT_LIBS with regard to
dnl the specified PREFIX and set with_expat to PREFIX, or 'yes' if PREFIX
dnl has not been specified. Also HAVE_LIBEXPAT, HAVE_EXPAT_H are defined.
dnl If --with-expat has not been specified, set with_expat to 'no'.
dnl In addition, an Automake conditional EXPAT_INSTALLED is set accordingly.
dnl This is necessary to adapt a whole lot of packages that have expat
dnl bundled as a static library.
AC_ARG_WITH([expat],[AS_HELP_STRING([[--with-expat[=DIR]]],[use Expat
  installed in DIR/include and DIR/lib])],,[with_expat=yes])

AM_CONDITIONAL([EXPAT_INSTALLED],[test xno != x"$with_expat"])

EXPAT_CFLAGS=
EXPAT_LIBS=
AS_IF([test xno != x"$with_expat"],
[AS_IF([test xyes != x"$with_expat"],[
  EXPAT_CFLAGS="-I$with_expat/include"
  EXPAT_LIBS="-L$with_expat/lib"
  AC_CHECK_LIB([expat], [XML_ParserCreate],
		     [ EXPAT_LIBS="$EXPAT_LIBS -lexpat"
		       expat_found=yes ],
		     [ expat_found=no ],
		     ["$EXPAT_LIBS"])
  AS_IF([test no = $expat_found],
        [AC_MSG_ERROR([Could not find the Expat library])])
  expat_save_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS $EXPAT_CFLAGS"
  AC_CHECK_HEADERS([expat.h], ,[expat_found=no])
  AS_IF([test no = $expat_found],
        [AC_MSG_ERROR([Could not find expat.h])])
  CFLAGS="$expat_save_CFLAGS"
])])

AC_SUBST([EXPAT_CFLAGS])
AC_SUBST([EXPAT_LIBS])

AH_TEMPLATE(GUILE_GC_MALLOC,
            [define guile gc-ed malloc function name (all peoples hate guile..)])

AC_CHECK_DECL(scm_gc_malloc, AC_DEFINE(GUILE_GC_MALLOC, scm_gc_malloc),
                             AC_DEFINE(GUILE_GC_MALLOC, scm_must_malloc),
                             [#include <libguile.h>])

AC_CONFIG_FILES([
  Makefile
  src/Makefile
  test/Makefile
  test/runtest
  doc/Makefile
])
AC_OUTPUT

dnl configure.ac ends here
