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

/*

  Coding conventions:

  - expat functions and datatypes start with XML_, and contain one or
  more capitalized words.

  - gexpat C functions and datatypes start with xml_, and contain
  the same words as their expat counterparts, but separated by
  underscores and not capitalized.

  - gexpat Scheme functions and datatypes start with expat:,
  and contain the same words as their C counterparts, but separated by
  hyphens and not capitalized.

  "void *" expat datatypes have usually been mapped to smobs.

*/
 

#include <expat.h>

#define GEXPAT_MODULE_NAME "xml expat"

void 
init_gexpat(void);

XML_Encoding *
xml_encoding_smob_to_expat(SCM xml_encoding_smob);

typedef struct {
  SCM data;

  short use_parser_as_handler_arg; /* 1 if the data if
				      xml_use_parser_as_handler_arg()
				      was called */
  SCM parser; /* defined only if the previous parameter is 1 */
  SCM external_entity_ref_handler_arg; /* only used when 
					  xml_set_external_entity_ref_arg()
					  is called */

  /* Maybe this structure is too big (could be implemented with a list
     which would contain only the implemented handlers ) */

  SCM start_element_handler;
  SCM end_element_handler;
  SCM character_data_handler;
  SCM processing_instruction_handler;
  SCM comment_handler;
  SCM start_cdata_section_handler;
  SCM end_cdata_section_handler;
  SCM default_handler;
  SCM default_handler_expand;
  SCM unparsed_entity_decl_handler;
  SCM notation_decl_handler;
  SCM start_namespace_decl_handler;
  SCM end_namespace_decl_handler;
  SCM not_standalone_handler;
  SCM external_entity_ref_handler;

} user_data;

/*
 * A structure used to wrap encoding handler data when calling
 * XML_SetUnknownEncodingHandler
 */ 
typedef struct {
  SCM data;     /* the encoding handler data provided by the Scheme caller */
  SCM handler;  /* a reference on the handler, used by
		   generic_unknown_encoding_handler */
} encoding_handler_data;
