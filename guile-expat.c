/*
 * The Original Code is Mixp.
 *
 * The Initial Developer of the Original Code is Thierry Bézecourt.
 * Portions created by Thierry Bézecourt are Copyright (C) 1999, 2000
 * Thierry Bézecourt. All Rights Reserved.
 * 
 * Copyright (C) 2002, 2003 Dmitry Morozhnikov <dmiceman@mail.ru>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 */

#include <stdio.h>

#include <libguile.h>

void main_prog(void *closure, int argc, char *argv[])
{
  if(argc == 1) {

    init_gexpat();
    scm_shell(argc, argv);

  }
  else if(argc == 2) {
   
    init_gexpat();
    scm_c_primitive_load(argv[1]);

  }
  else {
    fprintf(stderr, "Usage: %s [file]\n", argv[0]);
    exit(1);
  }

  exit(0);
}

int
main (int argc, char *argv[])
{
  scm_boot_guile(argc, argv, main_prog, NULL);

  return 0; /* never reached */
}
