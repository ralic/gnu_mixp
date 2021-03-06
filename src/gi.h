/* gi.h

   Copyright (C) 2004, 2005, 2006, 2008, 2009, 2011, 2012 Thien-Thi Nguyen

   This is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this package; see the file COPYING.  If not, write to the
   Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA  02110-1301  USA.  */

/*
 * always
 */

#ifndef _GI_H_
#define _GI_H_

#include <libguile.h>

#if defined SCM_MAJOR_VERSION && defined SCM_MINOR_VERSION
#define GI_LEVEL  ((SCM_MAJOR_VERSION << 8) + SCM_MINOR_VERSION)
#else
#define GI_LEVEL  0x0104                /* Guile 1.4.x */
#endif
#define GI_LEVEL_NOT_YET_1_8  (GI_LEVEL < 0x0108)

#if GI_LEVEL_NOT_YET_1_8

#include <guile/gh.h>
#define NULLP             gh_null_p
#define STRINGP           gh_string_p
#define BOOLEAN           gh_bool2scm
#define NUM_INT           gh_int2scm
#define SYMBOL            gh_symbol2scm
#define STRING            gh_str02scm
#define BSTRING           gh_str2scm
#define C_INT             gh_scm2int
#define VECREF            gh_vector_ref
#define EQ                gh_eq_p
#define CONS              gh_cons
#define CAR               gh_car
#define CDR               gh_cdr
#define APPLY             gh_apply
#define LISTIFY           gh_list
#define CALL0             gh_call0
#define CALL1             gh_call1
#define CALL2             gh_call2

#define DEFSMOB(tagvar,name,m,f,p)                              \
  tagvar = scm_make_smob_type_mfpe (name, 0, m, f, p, NULL)

#define GCMALLOC(sz,what)    scm_must_malloc (sz, what)
#define GCFREE(ptr,what)     scm_must_free (ptr)
#define GCRV(sz)             sz

#else  /* !GI_LEVEL_NOT_YET_1_8 */
#define NULLP             scm_is_null
#define BOOLEAN           scm_from_bool
#define NUM_INT           scm_from_int
#define SYMBOL            scm_from_locale_symbol
#define STRING            scm_from_locale_string
#define BSTRING           scm_from_locale_stringn
#define C_INT             scm_to_int
#define VECREF            scm_vector_ref
#define EQ                scm_is_eq
#define CONS              scm_cons
#define CAR               scm_car
#define CDR               scm_cdr
#define APPLY             scm_apply_0
#define LISTIFY           scm_list_n
#define CALL0             scm_call_0
#define CALL1             scm_call_1
#define CALL2             scm_call_2

#define DEFSMOB(tagvar,name,m,f,p)  do          \
    {                                           \
      tagvar = scm_make_smob_type (name, 0);    \
      if (NULL != m)                            \
        scm_set_smob_mark (tagvar, m);          \
      if (NULL != f)                            \
        scm_set_smob_free (tagvar, f);          \
      if (NULL != p)                            \
        scm_set_smob_print (tagvar, p);         \
    }                                           \
  while (0)

#define GCMALLOC(sz,what)    scm_gc_malloc (sz, what)
#define GCFREE(ptr,what)     scm_gc_free (ptr, sizeof (*(ptr)), what)
#define GCRV(sz)             0

#endif /* !GI_LEVEL_NOT_YET_1_8 */

/*
 * backward (sometimes foresight was incomplete)
 */

#ifdef HAVE_GUILE_MODSUP_H
#include <guile/modsup.h>
#else  /* !defined HAVE_GUILE_MODSUP_H */

#define GH_DEFPROC(fname, primname, req, opt, var, arglist, docstring) \
  SCM_SNARF_HERE (static SCM fname arglist;)                           \
  SCM_DEFINE (fname, primname, req, opt, var, arglist, docstring)

#define GH_MODULE_LINK_FUNC(module_name, fname_frag, module_init_func)  \
void                                                                    \
scm_init_ ## fname_frag ## _module (void);                              \
void                                                                    \
scm_init_ ## fname_frag ## _module (void)                               \
{                                                                       \
  /* Make sure strings(1) finds module name at bol.  */                 \
  static const char modname[] = "\n" module_name;                       \
  scm_register_module_xxx (1 + modname, module_init_func);              \
}

#endif  /* !defined HAVE_GUILE_MODSUP_H */

/*
 * abstractions
 */

#if GI_LEVEL_NOT_YET_1_8
#define NOINTS()   SCM_DEFER_INTS
#define INTSOK()   SCM_ALLOW_INTS
#else
#define NOINTS()
#define INTSOK()
#endif

#define GIVENP(x)          (! SCM_UNBNDP (x))
#define NOT_FALSEP(x)      (SCM_NFALSEP (x))

#define DEFAULT_FALSE(maybe,yes)  ((maybe) ? (yes) : SCM_BOOL_F)
#define RETURN_FALSE()                        return SCM_BOOL_F
#define RETURN_UNSPECIFIED()                  return SCM_UNSPECIFIED

#define ASSERT(what,expr,msg)  SCM_ASSERT ((expr), what, msg, FUNC_NAME)
#define ASSERT_STRING(n,arg)  ASSERT (arg, STRINGP (arg), SCM_ARG ## n)

#define SMOBDATA(obj)  ((void *) SCM_SMOB_DATA (obj))

#define PCHAIN(...)  (LISTIFY (__VA_ARGS__, SCM_UNDEFINED))

#define ERROR(blurb, ...)  SCM_MISC_ERROR (blurb, PCHAIN (__VA_ARGS__))
#define SYSTEM_ERROR()     SCM_SYSERROR

#define PRIMPROC             GH_DEFPROC
#define PERMANENT            scm_permanent_object
#define MOD_INIT_LINK_THUNK  GH_MODULE_LINK_FUNC

#endif /* _GI_H_ */

/* gi.h ends here */
