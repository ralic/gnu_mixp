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
#include "snuggle/level.h"
#include "snuggle/humdrum.h"
#include "snuggle/defsmob.h"
#include "snuggle/finangle.h"
#include "snuggle/modsup.h"

/*
 * abstractions
 */

#if ! GI_LEVEL_1_8
#define NOINTS()   SCM_DEFER_INTS
#define INTSOK()   SCM_ALLOW_INTS
#else
#define NOINTS()
#define INTSOK()
#endif

#define PROCP(x)           PROCEDUREP (x)

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

#endif /* _GI_H_ */

/* gi.h ends here */
