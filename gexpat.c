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

/* The interface. Defines all the expat: functions available, and
   some of the mixp: functions. */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include <libguile.h>

#include "gexpat.h"
#include "generic_handlers.h"

#include <config.h>

/*
#define MAX_ALLOC 10000000
char buf[MAX_ALLOC];
char* cursor = buf;

void *malloc(size_t size)
{
  void *res;

  printf("allocation à %018d de %d.\n", cursor-buf, size);

  if(cursor - buf >= MAX_ALLOC) {
    printf("Taille maximum d'allocation atteinte\n");
    abort();
  }
    
  res = (void*)cursor;
  cursor += size;

  return(res);
}

void 
free(void* ptr)
{
  printf("désallocation de %018d.\n", (char*)ptr - buf);
}
*/


/* Forward declarations */
user_data *
get_user_data(XML_Parser xml_parser);

void
mark_user_data(user_data* data);

/**********************************************************************
 * 
 * The xml-parser / xml_parser / XML_Parser type
 *
 **********************************************************************/

long xml_parser_tag;

typedef struct {
  XML_Parser p; /* The underlying expat object */
  short freed;  /* Becomes 1 when we call XML_ParserFree on p */
} mixp_xml_parser;

#define XML_PARSERP(smob) \
        (SCM_NIMP(smob) && SCM_CAR(smob) == (void *) xml_parser_tag)
/*= #define SMOB_TO_XML_PARSER(smob) \
 *=         (*(XML_Parser *)SCM_CDR(smob)) =*/
#define SMOB_TO_MIXP_XML_PARSER(smob) \
        ((mixp_xml_parser*)SCM_CDR(smob))
#define SMOB_TO_XML_PARSER(smob) \
        (SMOB_TO_MIXP_XML_PARSER(smob)->p)
#define SMOB_TO_XML_PARSER_PTR(smob) \
        (XML_Parser *)SCM_CDR(smob)

static SCM
mark_xml_parser(SCM obj)
{
/*    XML_Parser *xml_parser = (XML_Parser *)SCM_CDR(obj); */
  XML_Parser* xml_parser = SMOB_TO_XML_PARSER_PTR(obj);
  
  /* We must mark every SCM in the user data, or they might be GC'ed
     before they are used */
  user_data* ud = get_user_data(*xml_parser);
  mark_user_data(ud);

  return SCM_BOOL_F;
}

void
ensure_xml_parser_freed(SCM obj)
{
  mixp_xml_parser* m = SMOB_TO_MIXP_XML_PARSER(obj);
  
  if(! m->freed) {
    XML_Parser xml_parser = m->p;
    XML_ParserFree(xml_parser);
    m->freed = 1;
  }
}

static size_t
free_xml_parser(SCM obj)
{
/*=   XML_Parser *xml_parser = (XML_Parser *)SCM_CDR(obj); =*/

  size_t size = sizeof(mixp_xml_parser);

  ensure_xml_parser_freed(obj);

  free(SMOB_TO_MIXP_XML_PARSER(obj));

  return 0;
}

static int
print_xml_parser(SCM obj, SCM port, scm_print_state *pstate)
{
  char buf[20];
  scm_puts("#<xml-parser ", port);
  snprintf(buf, 19, "%d", *(XML_Parser *)SCM_CDR(obj));
  scm_puts(buf, port);
  scm_puts(">", port);
  return 1;
}

void
init_xml_parser_type()
{
  xml_parser_tag = scm_make_smob_type("xmlparser", 0);
  scm_set_smob_mark(xml_parser_tag, mark_xml_parser);
  scm_set_smob_free(xml_parser_tag, free_xml_parser);
  scm_set_smob_print(xml_parser_tag, print_xml_parser);
}

/* Used internally (not exported to the Scheme layer) */

static SCM
make_xml_parser_smob(XML_Parser xml_parser)
{
  SCM smob;
  /*XML_Parser* copy;*/
  mixp_xml_parser *copy;

  SCM_DEFER_INTS;
/*=   copy = (XML_Parser *)scm_must_malloc(sizeof(XML_Parser),
 *= 				       "XML_Parser"); =*/
  copy = (mixp_xml_parser *)GUILE_GC_MALLOC(sizeof(XML_Parser) + sizeof(short),
					    "mixp_xml_parser");
  SCM_ALLOW_INTS;
/*=   *copy = xml_parser; =*/
  copy->p = xml_parser;
  copy->freed = 0;

  SCM_NEWSMOB(smob, xml_parser_tag, copy);

  return smob;
}

/*
 * Common type checking and initialization of the XML parser object
 * xml_parser must point to an allocated XML_Parser instance
 */

/* 
 * mixp:parser? / xml_parser_p
 */
SCM_DEFINE(xml_parser_p, "mixp:parser?", 1, 0, 0, (SCM obj), "")
#define FUNC_NAME xml_parser_p
{
  return (XML_PARSERP(obj) ? SCM_BOOL_T : SCM_BOOL_F);
}
#undef FUNC_NAME

void
init_xml_parser0(SCM parser, char *subr_name, 
		 XML_Parser* xml_parser)
{
  SCM_ASSERT(XML_PARSERP(parser), parser, 
	     SCM_ARG1, subr_name);

  *xml_parser = SMOB_TO_XML_PARSER(parser);
}

void
init_xml_parser1(SCM parser, SCM handler, char *subr_name, 
		 XML_Parser *xml_parser)
{
  SCM_ASSERT(XML_PARSERP(parser), parser, 
	     SCM_ARG1, subr_name);
  SCM_ASSERT(SCM_NIMP(handler) && SCM_CLOSUREP(handler), handler,
	     SCM_ARG2, subr_name);
  
  *xml_parser = SMOB_TO_XML_PARSER(parser);
}

void
init_xml_parser2(SCM parser, SCM handler1, SCM handler2, char *subr_name, 
		 XML_Parser *xml_parser)
{
  SCM_ASSERT(XML_PARSERP(parser), parser, 
	     SCM_ARG1, subr_name);

  /* When two handlers are specified, each one must be either closures
     or #f */
  SCM_ASSERT((SCM_NIMP(handler1) && SCM_CLOSUREP(handler1))
	     || !SCM_NFALSEP(handler1),
	     handler1, SCM_ARG2, subr_name);
  SCM_ASSERT((SCM_NIMP(handler2) && SCM_CLOSUREP(handler2))
	     || !SCM_NFALSEP(handler2),
	     handler2, SCM_ARG3, subr_name);
  
  *xml_parser = SMOB_TO_XML_PARSER(parser);
}

/*
 * The user_data structure, a wrapper around the userData parameter
 * in expat functions
 */
user_data *
alloc_user_data()
{
  user_data *res;

  SCM_DEFER_INTS;
  res = (user_data *)malloc(sizeof(user_data));
  SCM_ALLOW_INTS;

  /* Initialize the user data to SCM_UNSPECIFIED (because it is a
     valid value in Scheme), and the handlers to SCM_UNDEFINED (so
     that SCM_UNBNDP will return ture when the handler has not been
     set) */

  res->data = SCM_UNSPECIFIED;

  res->use_parser_as_handler_arg = 0;
  res->parser                          = SCM_UNDEFINED;
  res->external_entity_ref_handler_arg = SCM_UNDEFINED;

  res->start_element_handler	      = SCM_UNDEFINED;
  res->end_element_handler	      = SCM_UNDEFINED;
  res->character_data_handler	      = SCM_UNDEFINED;
  res->processing_instruction_handler = SCM_UNDEFINED;
  res->comment_handler		      = SCM_UNDEFINED;
  res->start_cdata_section_handler    = SCM_UNDEFINED;
  res->end_cdata_section_handler      = SCM_UNDEFINED;
  res->default_handler		      = SCM_UNDEFINED;
  res->default_handler_expand	      = SCM_UNDEFINED;
  res->unparsed_entity_decl_handler   = SCM_UNDEFINED;
  res->notation_decl_handler	      = SCM_UNDEFINED;
  res->start_namespace_decl_handler   = SCM_UNDEFINED;
  res->end_namespace_decl_handler     = SCM_UNDEFINED;
  res->not_standalone_handler	      = SCM_UNDEFINED;
  res->external_entity_ref_handler    = SCM_UNDEFINED;

  return res;
}

/*
 * Called by the `mark' function of the xml_parser smob type.
 */
void
mark_user_data(user_data *data)
{
  /* Call scm_gc_mark on every SCM element in the structure */
  scm_gc_mark(data->data);

  if(!SCM_UNBNDP(data->parser))
    scm_gc_mark(data->parser);
  if(!SCM_UNBNDP(data->external_entity_ref_handler_arg))
    scm_gc_mark(data->external_entity_ref_handler_arg);

  if(!SCM_UNBNDP(data->start_element_handler))
    scm_gc_mark(data->start_element_handler);
  if(!SCM_UNBNDP(data->end_element_handler))
    scm_gc_mark(data->end_element_handler);
  if(!SCM_UNBNDP(data->character_data_handler))
    scm_gc_mark(data->character_data_handler);
  if(!SCM_UNBNDP(data->comment_handler))
    scm_gc_mark(data->comment_handler);
  if(!SCM_UNBNDP(data->start_cdata_section_handler))
    scm_gc_mark(data->start_cdata_section_handler);
  if(!SCM_UNBNDP(data->end_cdata_section_handler))
    scm_gc_mark(data->end_cdata_section_handler);
  if(!SCM_UNBNDP(data->default_handler))
    scm_gc_mark(data->default_handler);
  if(!SCM_UNBNDP(data->default_handler_expand))
    scm_gc_mark(data->default_handler_expand);
  if(!SCM_UNBNDP(data->unparsed_entity_decl_handler))
    scm_gc_mark(data->unparsed_entity_decl_handler);
  if(!SCM_UNBNDP(data->notation_decl_handler))
    scm_gc_mark(data->notation_decl_handler);
  if(!SCM_UNBNDP(data->start_namespace_decl_handler))
    scm_gc_mark(data->start_namespace_decl_handler);
  if(!SCM_UNBNDP(data->end_namespace_decl_handler))
    scm_gc_mark(data->end_namespace_decl_handler);
  if(!SCM_UNBNDP(data->not_standalone_handler))
    scm_gc_mark(data->not_standalone_handler);
  if(!SCM_UNBNDP(data->external_entity_ref_handler))
    scm_gc_mark(data->external_entity_ref_handler);
}

void
free_user_data(user_data *data)
{
  free(data);
  /* do not free the parser ! */
}

/*
 * Return the user_data element which is associated with each XML_Parser
 *  object. Creates it if necessary.
 */
user_data *
get_user_data(XML_Parser xml_parser)
{
  void* res = XML_GetUserData(xml_parser);

  if(res == NULL) {
    res = (void *)alloc_user_data();
    XML_SetUserData(xml_parser, res);
  }
  
  return (user_data *)res;
}


/**********************************************************************
 * 
 * The xml-encoding / xml_encoding / XML_Encoding type
 *
 **********************************************************************/

long xml_encoding_tag;

/* A structure used to wrap the data element of a XML_Encoding
   structure */
typedef struct {  
  int* map;     /* a reference on the map in the XML_Encoding object,
		   here only because generic_xml_encoding_convert
		   needs it ; do not free it, it's really an alias for
		   the map in the XML_Encoding object */
  SCM data;     /* The user data provided by the user */
  SCM convert;  /* A conversion function for multi-byte sequences */
  SCM release;  /* A release function called when the encoding is not
		   used any more */
} xml_encoding_data;

static SCM 
mark_xml_encoding(SCM obj)
{
  XML_Encoding* xml_encoding = (XML_Encoding *)SCM_CDR(obj);
  xml_encoding_data* xed = (xml_encoding_data *)xml_encoding->data;
  
  scm_gc_mark(xed->data);
  scm_gc_mark(xed->convert);

  return xed->release;
}

static size_t
free_xml_encoding(SCM obj)
{
  XML_Encoding* xml_encoding = (XML_Encoding *)SCM_CDR(obj);
  size_t size = sizeof(XML_Encoding) + sizeof(xml_encoding_data);

  free(xml_encoding->data);
  free(xml_encoding);

  return 0;
}

static int
print_xml_encoding(SCM obj, SCM port, scm_print_state* pstate)
{
  XML_Encoding* xml_encoding = (XML_Encoding *)SCM_CDR(obj);
  int i;

  scm_puts("#<xml-encoding", port);

  for(i=0; i < 256; i++) {
    scm_puts(" ",  port);
    scm_display(SCM_MAKINUM(xml_encoding->map[i]), port);
  }

  scm_puts(">", port);

  /* non-zero means success */
  return 1;
}

/*
 * Implements the convert() function in XML_Encoding.
 */
static int
generic_xml_encoding_convert(void *data, const char *s)
{
  xml_encoding_data* xed = (xml_encoding_data *)data;
  return gh_call2(xed->convert,
		  xed->data,
		  scm_mem2string(s,
				 xed->map[(unsigned char)*s])); /* s's length */
}

void
generic_xml_encoding_release(void *data)
{
  xml_encoding_data* xed = (xml_encoding_data *)data;
  gh_call1(xed->release, xed->data);
}

XML_Encoding *
xml_encoding_smob_to_expat(SCM xml_encoding_smob)
{
  /* TBD: type checking */
  return (XML_Encoding *)SCM_CDR(xml_encoding_smob);
}

/*
 * SCM API: make a XML_Encoding
 */ 
SCM_DEFINE(make_xml_encoding, "make-xml-encoding", \
           4, 0, 0, (SCM map, SCM data, SCM convert, SCM release), "")
#define FUNC_NAME make_xml_encoding
{
  XML_Encoding* xml_encoding = NULL;
  SCM xml_encoding_smob;
  SCM* map_elts;
  int data_len;
  xml_encoding_data *xe_data;
  int i;
 
  SCM_ASSERT(SCM_NIMP(map) && SCM_VECTORP(map) && SCM_VECTOR_LENGTH(map) == 256,
	     map, SCM_ARG1, "make-xml-encoding");
  /* no check on data */
  SCM_ASSERT(SCM_NIMP(convert) && SCM_CLOSUREP(convert),
	     convert, SCM_ARG1, "make-xml-encoding");
  SCM_ASSERT(SCM_NIMP(release) && SCM_CLOSUREP(release),
	     release, SCM_ARG1, "make-xml-encoding");
    
  SCM_DEFER_INTS;
  xml_encoding = (XML_Encoding *)GUILE_GC_MALLOC(sizeof(XML_Encoding),
						 "XML_Encoding");
  SCM_ALLOW_INTS;

  /* The map goes directly into the XML_Encoding object*/
  map_elts = (SCM *) SCM_VELTS(map);
  for(i=0; i < 256; i++)
    xml_encoding->map[i] = SCM_INUM(map_elts[i]);

  /* The user data, which we encapsulate into our own layer, which
     also contains the convert and release functions */
  SCM_DEFER_INTS;
  xml_encoding->data = GUILE_GC_MALLOC(sizeof(xml_encoding_data),
				       "xml_encoding_data");
  SCM_ALLOW_INTS;

  xe_data = (xml_encoding_data *)xml_encoding->data;
  xe_data->map = xml_encoding->map;
  xe_data->data = data;
  xe_data->convert = convert;
  xe_data->release = release;

  /* The convert() and release() functions are replaced by generic
     versions which will call the user-specified versions */
  xml_encoding->convert = generic_xml_encoding_convert;
  xml_encoding->release = generic_xml_encoding_release;
  
  SCM_NEWSMOB(xml_encoding_smob, xml_encoding_tag, xml_encoding);

  return xml_encoding_smob;
}
#undef FUNC_NAME

/* TBD: define an equal? function ? */

static void
init_xml_encoding_type()
{
  xml_encoding_tag = scm_make_smob_type("xmlencoding", 0);
  scm_set_smob_free(xml_encoding_tag, free_xml_encoding);
  scm_set_smob_print(xml_encoding_tag, print_xml_encoding);

  scm_c_make_gsubr("make-xml-encoding", 4, 0, 0, make_xml_encoding);
}

/**********************************************************************
 *
 * EXPAT INTERFACE
 *
 **********************************************************************/

/*
 * expat:parser-create
 * Returns a new XML_Parser object.
 */
SCM_DEFINE(xml_parser_create, "expat:parser-create", \
           0, 1, 0, (SCM encoding), "")
#define FUNC_NAME xml_parser_create
{
  XML_Parser xml_parser;
  SCM res;
  XML_Char* e;

  SCM_ASSERT(SCM_UNBNDP(encoding) ||
	     (SCM_NIMP(encoding) && SCM_STRINGP(encoding)), 
	     encoding, SCM_ARG1, "expat:parser-create");
  
  e = SCM_UNBNDP(encoding) ? NULL : SCM_STRING_CHARS(encoding);
  xml_parser = XML_ParserCreate(e);
  res = make_xml_parser_smob(xml_parser);

  return res;
}
#undef FUNC_NAME

/*
 * expat:parser-create-ns / xml_parser_create_ns / XML_ParserCreateNS
 */
SCM_DEFINE(xml_parser_create_ns, "expat:parser-create-ns", \
           2, 0, 0, (SCM encoding, SCM namespace_separator), "")
#define FUNC_NAME xml_parser_create_ns
{
  TRACE_ENTER("xml_parser_create_ns");

  XML_Parser xml_parser;
  SCM res;
  XML_Char *e;
  XML_Char c;

  SCM_ASSERT(SCM_UNBNDP(encoding) ||
	     (SCM_NIMP(encoding) && SCM_STRINGP(encoding)),
	     encoding, SCM_ARG1, "expat:parser-create-ns");
  SCM_ASSERT(SCM_UNBNDP(namespace_separator) ||
	     SCM_CHARP(namespace_separator), namespace_separator,
	     SCM_ARG2, "expat:parser:create-ns");

  e = (SCM_UNBNDP(encoding) ? NULL : SCM_STRING_CHARS(encoding));
  c = (SCM_UNBNDP(namespace_separator) ?
       '\0' : SCM_CHAR(namespace_separator));

  xml_parser = XML_ParserCreateNS(e, c);
  res = make_xml_parser_smob(xml_parser);
  
  TRACE_EXIT();
  return res;
}
#undef FUNC_NAME

/*
 * expat:set-element-handler / xml_set_element_handler /
 * XML_SetElementHandler 
 * If you only want to set one of the two handlers, specify #f for the
 other */ 
SCM_DEFINE(xml_set_element_handler, "expat:set-element-handler", \
           3, 0, 0, (SCM parser, SCM start, SCM end), "")
#define FUNC_NAME xml_set_element_handler
{
  TRACE_ENTER("xml_set_element_handler");

  XML_Parser xml_parser;
  user_data* ud;

  init_xml_parser2(parser, start, end, "expat:set-element-handler",
		   &xml_parser);
 
  ud = get_user_data(xml_parser);
  
  if(SCM_NFALSEP(start))
    ud->start_element_handler = start;
  
  if(SCM_NFALSEP(end))
    ud->end_element_handler = end;
  
  XML_SetElementHandler(xml_parser, 
			generic_start_handler, generic_end_handler);

  TRACE_EXIT();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/*
 * expat:set-character-data-handler / xml_set_character_data_handler /
 * XML_SetCharacterDataHandler
 */
SCM_DEFINE(xml_set_character_data_handler, \
           "expat:set-character-data-handler", \
           2, 0, 0, (SCM parser, SCM handler), "")
#define FUNC_NAME xml_set_character_data_handler
{
  TRACE_ENTER("xml_character_data_handler");
  XML_Parser xml_parser;
  init_xml_parser1(parser, handler, "expat:character-data-handler",
		   &xml_parser);
  get_user_data(xml_parser)->character_data_handler = handler;
  
  XML_SetCharacterDataHandler(xml_parser, 
			      generic_character_data_handler);
  
  TRACE_EXIT();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/*
 * expat:set-processing-instruction-handler / 
 * xml_set_processing_instruction_handler /
 * XML_ProcessingInstructionHandler
 */
SCM_DEFINE(xml_set_processing_instruction_handler, \
           "expat:set-processing-instruction-handler", \
           2, 0, 0, (SCM parser, SCM handler), "")
#define FUNC_NAME xml_set_processing_instruction_handler
{
  TRACE_ENTER("xml_set_processing_instruction_handler");
  XML_Parser xml_parser;
  init_xml_parser1(parser, handler, 
		   "expat:set-processing-instruction-handler",
		   &xml_parser);
  get_user_data(xml_parser)->processing_instruction_handler = handler;

  XML_SetProcessingInstructionHandler(xml_parser,
				      generic_processing_instruction_handler);
  TRACE_EXIT();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/*
 * expat:set-comment-handler / 
 * xml_set_comment_handler /
 * XML_ Handler
 */
SCM_DEFINE(xml_set_comment_handler, "expat:set-comment-handler", \
           2, 0, 0, (SCM parser, SCM handler), "")
#define FUNC_NAME xml_set_comment_handler
{
  TRACE_ENTER("xml_comment_handler");
  XML_Parser xml_parser;
  user_data *ud;
  init_xml_parser1(parser, handler, "expat:set-comment-handler",
		   &xml_parser);
  ud = get_user_data(xml_parser);
  ud->comment_handler = handler;
  XML_SetCommentHandler(xml_parser,
			generic_comment_handler);
 
  TRACE_EXIT();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/*
 * expat:set-cdata-section-handler / 
 * xml_set_cdata_section_handler /
 * XML_SetCdataSectionHandler
 */
SCM_DEFINE(xml_set_cdata_section_handler, "expat:set-cdata-section-handler",  \
           3, 0, 0, (SCM parser, SCM start, SCM end), "")
#define FUNC_NAME xml_set_cdata_section_handler
{
  TRACE_ENTER("xml_set_cdata_section_handler");

  XML_Parser xml_parser;
  user_data* ud;

  init_xml_parser2(parser, start, end, "expat:set-cdata-section-handler",
		   &xml_parser);
  
  ud = get_user_data(xml_parser);

  if(SCM_NFALSEP(start))
    ud->start_cdata_section_handler = start;

  if(SCM_NFALSEP(end))
    ud->end_cdata_section_handler = end;

  XML_SetCdataSectionHandler(xml_parser, 
			     generic_start_cdata_section_handler, 
			     generic_end_cdata_section_handler);

  TRACE_EXIT();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/*
 * expat:set-default-handler / 
 * xml_set_default_handler /
 * XML_SetDefaultHandler
 */
SCM_DEFINE(xml_set_default_handler, "expat:set-default-handler", \
           2, 0, 0, (SCM parser, SCM handler), "")
#define FUNC_NAME xml_set_default_handler
{
  TRACE_ENTER("xml_default_handler");
  XML_Parser xml_parser;
  init_xml_parser1(parser, handler, "expat:set-default-handler",
		   &xml_parser);
  get_user_data(xml_parser)->default_handler = handler;
  XML_SetDefaultHandler(xml_parser,
			generic_default_handler);
  TRACE_EXIT();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/*
 * expat:set-default-handler-expand / xml_set_default_handler_expand /-
 * XML_SetDefaultHandlerExpand
 */
SCM_DEFINE(xml_set_default_handler_expand, \
           "expat:set-default-handler-expand", \
           2, 0, 0, (SCM parser, SCM handler), "")
#define FUNC_NAME xml_set_default_handler_expand
{
  TRACE_ENTER("xml_set_default_handler_expand");
  XML_Parser xml_parser;
  init_xml_parser1(parser, handler, "expat:set-default-handler-expand",
		   &xml_parser);
  XML_SetDefaultHandlerExpand(xml_parser,
			      generic_default_handler_expand);
  get_user_data(xml_parser)->default_handler_expand = handler;
  TRACE_EXIT();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/*
 * expat:set-unparsed-entity-decl-handler / 
 * xml_set_unparsed_entity_decl_handler /
 * XML_SetUnparsedEntityDeclHandler
 */
SCM_DEFINE(xml_set_unparsed_entity_decl_handler, \
           "expat:set-unparsed-entity-decl-handler", \
           2, 0, 0, (SCM parser, SCM handler), "")
#define FUNC_NAME xml_set_unparsed_entity_decl_handler
{
  TRACE_ENTER("xml_unparsed_entity_decl_handler");
  XML_Parser xml_parser;
  init_xml_parser1(parser, handler, 
		   "expat:set-unparsed-entity-decl-handler",
		   &xml_parser);
  get_user_data(xml_parser)->unparsed_entity_decl_handler = handler;

  XML_SetUnparsedEntityDeclHandler(xml_parser,
				   generic_unparsed_entity_decl_handler);
  TRACE_EXIT();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/*
 * expat:set-notation-decl-handler / 
 * xml_set_notation_decl_handler /
 * XML_SetNotationDeclHandler
 */
SCM_DEFINE(xml_set_notation_decl_handler, "expat:set-notation-decl-handler", \
           2, 0, 0, (SCM parser, SCM handler), "")
#define FUNC_NAME xml_set_notation_decl_handler
{
  TRACE_ENTER("xml_set_notation_decl_handler");

  XML_Parser xml_parser;
  init_xml_parser1(parser, handler, "expat:set-notation-decl-handler",
		   &xml_parser);
  get_user_data(xml_parser)->notation_decl_handler = handler;
  XML_SetNotationDeclHandler(xml_parser,
			     generic_notation_decl_handler);
  TRACE_EXIT();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/*
 * expat:set-namespace-decl-handler / 
 * xml_set_namespace_decl_handler /
 * XML_SetNamespaceHandler
 */
SCM_DEFINE(xml_set_namespace_decl_handler, \
           "expat:set-namespace-decl-handler", \
           3, 0, 0, (SCM parser, SCM start, SCM end), "")
#define FUNC_NAME xml_set_namespace_decl_handler
{
  TRACE_ENTER("xml_namespace_decl_handler");

  XML_Parser xml_parser;
  user_data* ud;

  init_xml_parser2(parser, start, end, "expat:set-namespace-decl-handler",
		   &xml_parser);
  
  ud = get_user_data(xml_parser);

  if(SCM_NFALSEP(start))
    ud->start_namespace_decl_handler = start;

  if(SCM_NFALSEP(end))
    ud->end_namespace_decl_handler = end;

  XML_SetNamespaceDeclHandler(xml_parser, 
			      generic_start_namespace_decl_handler, 
			      generic_end_namespace_decl_handler);

  TRACE_EXIT();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/*
 * expat:set-not-standalone-handler / 
 * xml_set_not_standalone_handler /
 * XML_SetNotStandaloneHandler
 */
SCM_DEFINE(xml_set_not_standalone_handler, \
           "expat:set-not-standalone-handler", \
           2, 0, 0, (SCM parser, SCM handler), "")
#define FUNC_NAME xml_set_not_standalone_handler
{
  TRACE_ENTER("xml_not_standalone_handler");
  XML_Parser xml_parser;
  init_xml_parser1(parser, handler, 
		   "expat:set-not_standalone-handler",
		   &xml_parser);
  get_user_data(xml_parser)->not_standalone_handler = handler;
  XML_SetNotStandaloneHandler(xml_parser,
			      generic_not_standalone_handler);
  TRACE_EXIT();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/*
 * expat:set-external-entity-ref-handler / 
 * xml_set_external_entity_ref_handler /
 * XML_SetExternalEntityRefHandler 
 */
SCM_DEFINE(xml_set_external_entity_ref_handler, \
           "expat:set-external-entity-ref-handler", \
           2, 0, 0, (SCM parser, SCM handler), "")
#define FUNC_NAME xml_set_external_entity_ref_handler
{
  TRACE_ENTER("xml_set_external_entity_ref_handler");
  XML_Parser xml_parser;
  user_data* ud;
  init_xml_parser1(parser, handler, 
		   "expat:set-external-entity-ref-handler",
		   &xml_parser);

  /* Store special information in the user data */
  ud = get_user_data(xml_parser);
  ud->external_entity_ref_handler = handler;
  ud->parser = parser;

  XML_SetExternalEntityRefHandler(xml_parser,
				  generic_external_entity_ref_handler);

  TRACE_EXIT();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/*
 * expat:set-external-entity-ref-handler-arg / 
 * xml_set_external_entity_ref_handler_arg /
 * XML_SetExternalEntityRefHandlerArg 
 *
 * Note: implemented without calling
 * XML_SetExternalEntityRefHandlerArg(), because the handler would
 * have no way to determine the type of the argument it is receiving 
 */
SCM_DEFINE(xml_set_external_entity_ref_handler_arg, \
           "expat:set-external-entity-ref-handler-arg", \
           3, 0, 0, (SCM parser, SCM handler, SCM arg), "")
#define FUNC_NAME xml_set_external_entity_ref_handler_arg
{
  TRACE_ENTER("xml_set_external_entity_ref_handler_arg");
  XML_Parser xml_parser;
  user_data *ud;
  init_xml_parser1(parser, handler, 
		   "expat:set-external-entity-ref-handler-arg",
		   &xml_parser);

  /* Store special information in the user data */
  ud = get_user_data(xml_parser);
  ud->external_entity_ref_handler = handler;
  ud->external_entity_ref_handler_arg = arg;
  XML_SetExternalEntityRefHandler(xml_parser,
				  generic_external_entity_ref_handler);

  TRACE_EXIT();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/*
 * expat:set-unknown-encoding-handler / xml_set_unknown_encoding_handler /
 * XML_SetUnknownEncodingHandler
 */

SCM_DEFINE(xml_set_unknown_encoding_handler, \
           "expat:set-unknown-encoding-handler", \
           3, 0, 0, (SCM parser, SCM handler, SCM data), "")
#define FUNC_NAME xml_set_unknown_encoding_handler
{
  TRACE_ENTER("xml_set_unknown_encoding_handler");
  XML_Parser xml_parser;

  encoding_handler_data *ehd;

  SCM_DEFER_INTS;
  ehd = (encoding_handler_data *)
    GUILE_GC_MALLOC(sizeof(encoding_handler_data), "encoding handler data");
  SCM_ALLOW_INTS;

  init_xml_parser1(parser, handler, "expat:set-unknown-encoding-handler",
		   &xml_parser);

  ehd->data = data;
  ehd->handler = handler;
  
  XML_SetUnknownEncodingHandler(xml_parser,
				generic_unknown_encoding_handler,
				ehd);

  TRACE_EXIT();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* 
 * expat:default-current / xml_default_current 
 * XML_DefaultCurrent
 */
SCM_DEFINE(xml_default_current, "expat:default-current", \
           1, 0, 0, (SCM parser), "")
#define FUNC_NAME xml_default_current
{
  TRACE_ENTER("xml_default_current");

  XML_Parser xml_parser;
  init_xml_parser0(parser, "expat:default-current", &xml_parser);

  XML_DefaultCurrent(xml_parser);

  TRACE_EXIT();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/*
 * expat:set-user-data / xml_set_user_data / XML_SetUserData
 */
SCM_DEFINE(xml_set_user_data, "expat:set-user-data", \
           2, 0, 0, (SCM parser, SCM data), "")
#define FUNC_NAME xml_set_user_data
{
  TRACE_ENTER("xml_set_user_data");

  XML_Parser xml_parser;
  user_data *ud;
 
  init_xml_parser0(parser, "expat:set-user-data", &xml_parser);

  ud = get_user_data(xml_parser);
  ud->data = data;

  XML_SetUserData(xml_parser, (void *)ud);

  TRACE_EXIT();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/*
 * expat:get-user-data / xml_get_user_data / XML_GetUserData
 * The Scheme object returned by this function will be the same as the
 * one which was set by xml-set-user-data (eqv? will return true)
 */
SCM_DEFINE(xml_get_user_data, "expat:get-user-data", \
           1, 0, 0, (SCM parser), "")
#define FUNC_NAME xml_get_user_data
{
  TRACE_ENTER("xml_get_user_data");
  
  XML_Parser xml_parser;
  void *xml_user_data;
  SCM res;

  init_xml_parser0(parser, "expat:get-user-data", &xml_parser);

  xml_user_data = XML_GetUserData(xml_parser);

  if(xml_user_data == NULL)
    res = SCM_UNSPECIFIED;
  else {
    /* If the user data is not null, it was set by xml_set_user_data()
       or get_user_data(), and its type is necessarily user_data */
    res = ((user_data *)xml_user_data)->data;
  }

  TRACE_EXIT();
  return res;
}
#undef FUNC_NAME

SCM_DEFINE(xml_set_encoding, "expat:set-encoding", \
           2, 0, 0, (SCM parser, SCM encoding), "")
#define FUNC_NAME xml_set_encoding
{
  TRACE_ENTER("xml_set_encoding");
  int res;

  XML_Parser xml_parser;

  init_xml_parser0(parser, "expat:xml-set-encoding", &xml_parser);
  SCM_ASSERT(SCM_NIMP(encoding) && SCM_STRINGP(encoding), 
	     encoding, SCM_ARG1, "expat:xml-set-encoding");
  
  res = XML_SetEncoding(xml_parser, SCM_STRING_CHARS(encoding));

  TRACE_EXIT();
  return SCM_MAKINUM(res);
}
#undef FUNC_NAME

/*
 expat:use-parser-as-handler-arg /
 xml_use_parser_as_handler_arg / XML_UseParserAsHandlerArg
*/

SCM_DEFINE(xml_use_parser_as_handler_arg, "expat:use-parser-as-handler-arg", \
           1, 0, 0, (SCM parser), "")
#define FUNC_NAME xml_use_parser_as_handler_arg
{
  TRACE_ENTER("xml_user_parser_as_handler_arg");

  XML_Parser xml_parser;
  user_data *ud;

  init_xml_parser0(parser, "expat:use-parser-as-handler-arg", 
		   &xml_parser);

  /* Do not call the expat function, because our generic handlers
     would have no way to know that they are receiving a parser and
     not a user data */
  ud = get_user_data(xml_parser);
  ud->use_parser_as_handler_arg = 1;
  ud->parser = parser;
  
  TRACE_EXIT();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/*
  * expat:set-base / xml_set_base / XML_SetBase
  */

SCM_DEFINE(xml_set_base, "expat:set-base", \
           2, 0, 0, (SCM parser, SCM base), "")
#define FUNC_NAME xml_set_base
{
  TRACE_ENTER("xml_set_base");

  int res;
  XML_Parser xml_parser;
  init_xml_parser0(parser, "expat:set-base", &xml_parser);
  SCM_ASSERT(SCM_NIMP(base) && SCM_STRINGP(base), 
	     base, SCM_ARG1, "expat:xml-set-base");

  res = XML_SetBase(xml_parser, SCM_STRING_CHARS(base));

  TRACE_EXIT();
  return SCM_MAKINUM(res);
}
#undef FUNC_NAME

/* 
 * expat:get-base / xml_get_base / XML_getBase
 */
SCM_DEFINE(xml_get_base, "expat:get-base", \
           1, 0, 0, (SCM parser), "")
#define FUNC_NAME xml_get_base
{
  TRACE_ENTER("xml_get_base");

  SCM res;
  const char *base;
  XML_Parser xml_parser;
  init_xml_parser0(parser, "expat:get-base", &xml_parser);

  base = XML_GetBase(xml_parser);
  if(base == NULL)
    res = SCM_UNSPECIFIED;
  else 
    res = scm_makfrom0str(base);

  TRACE_EXIT();
  return res;
}
#undef FUNC_NAME

/* 
 * expat:get-specified-attribute-count /
 * xml_get_specified_attribute_count /
 * XML_GetSpecifiedAttributeCount
 */
SCM_DEFINE(xml_get_specified_attribute_count, \
           "expat:get-specified-attribute-count", \
           1, 0, 0, (SCM parser), "")
#define FUNC_NAME xml_get_specified_attribute_count
{
  TRACE_ENTER("xml_get_specified_attribute_count");
  int res;
  XML_Parser xml_parser;

  init_xml_parser0(parser, "expat:get-specified-attribute-count",
		   &xml_parser);

  res = XML_GetSpecifiedAttributeCount(xml_parser);
  
  TRACE_EXIT();
  return SCM_MAKINUM(res);
}
#undef FUNC_NAME

/*
 * TBD: create a xml_parser type
 * expat:parse / xml_parse / XML_Parse
 */
SCM_DEFINE(xml_parse, "expat:parse", \
           3, 0, 0, (SCM parser, SCM s, SCM is_final), "")
#define FUNC_NAME xml_parse
{
  TRACE_ENTER("xml_parse");

  int res = 0;
  XML_Parser xml_parser;
  char* str;
  
  init_xml_parser0(parser, "expat:parse", &xml_parser);

  SCM_ASSERT(SCM_NIMP(s) && SCM_STRINGP(s), s, SCM_ARG2, "expat:parse");
  SCM_ASSERT((SCM_BOOL_F == is_final) || (SCM_BOOL_T == is_final), 
	     is_final, SCM_ARG3, "expat:parse");

  TRACE3("parser %d, string %s, is_final %d.\n",
	 xml_parser, SCM_STRING_CHARS(s),
	 SCM_NFALSEP(is_final));

  str = SCM_STRING_CHARS(s);
  res = XML_Parse(xml_parser,
		  str, strlen(str), SCM_NFALSEP(is_final));
  
  TRACE_EXIT();
  return SCM_MAKINUM(res);
}
#undef FUNC_NAME

/* 
 * TBD
void XMLPARSEAPI *
XML_GetBuffer(XML_Parser parser, int len);
*/

/*
 * expat:parse-buffer / xml_parse_buffer / XML_ParseBuffer
 */
SCM_DEFINE(xml_parse_buffer, "expat:parse-buffer", \
           3, 0, 0, (SCM parser, SCM len, SCM isFinal), "")
#define FUNC_NAME xml_parse_buffer
{
  TRACE_ENTER("xml_parse_buffer");
  int res;
  XML_Parser xml_parser;
  int l;
  init_xml_parser0(parser, "expat:parse-buffer", &xml_parser);
  SCM_ASSERT(SCM_INUMP(len), len, SCM_ARG2, "expat:parse-buffer");
  SCM_ASSERT(SCM_UNBNDP(isFinal) || SCM_INUMP(isFinal), 
	     isFinal, SCM_ARG3, "expat:parse-buffer");
  l = SCM_UNBNDP(len) ? SCM_INUM(len) : 0;

  res = XML_ParseBuffer(xml_parser, l, SCM_INUM(isFinal));

  TRACE_EXIT();
  return SCM_MAKINUM(res);
}
#undef FUNC_NAME

static struct ParamEntityParsingCodeInfo {
  int expat_code;
  char* symbol;
} param_entity_parsing_code_info[] =
{
  { XML_PARAM_ENTITY_PARSING_NEVER,
    "expat:XML_PARAM_ENTITY_PARSING_NEVER" },
  { XML_PARAM_ENTITY_PARSING_UNLESS_STANDALONE, 
    "expat:XML_PARAM_ENTITY_PARSING_UNLESS_STANDALONE" },
  { XML_PARAM_ENTITY_PARSING_ALWAYS,
    "expat:XML_PARAM_ENTITY_PARSING_ALWAYS" }
};

static int 
param_entity_parsing_codes_count = 
sizeof(param_entity_parsing_code_info) / (sizeof(int) + sizeof(char*));

int
param_entity_parsing_symbol_to_int(SCM code)
{
  char* symbol;
  int i;

  symbol = (char*)gh_symbol2newstr(code, NULL);

  for(i = 0; i < param_entity_parsing_codes_count; i++) {
    if(strcmp(param_entity_parsing_code_info[i].symbol, symbol) == 0) {
      free(symbol);
      return param_entity_parsing_code_info[i].expat_code;
    }
  }

  free(symbol);
  return -1;
}

/*
 * xml_set_param_entity_parsing / expat:set-param-entity-parsing /
 * XML_SetParamEntityParsing
 * Controls parsing of parameter entities (including the external DTD
 * subset).  See xmlparse.h for a complete description.
 * code is a Scheme symbol.
*/ 
SCM_DEFINE(xml_set_param_entity_parsing, "expat:set-param-entity-parsing", \
           2, 0, 0, (SCM parser, SCM code), "")
#define FUNC_NAME xml_set_param_entity_parsing
{
  TRACE_ENTER("xml_set_param_entity_parsing");

  XML_Parser xml_parser;
  int res;
  int intcode;

  SCM_ASSERT(XML_PARSERP(parser), parser, 
	     SCM_ARG1, "expat:set-param-entity-parsing");
  xml_parser = SMOB_TO_XML_PARSER(parser);
  intcode = param_entity_parsing_symbol_to_int(code);

  res = XML_SetParamEntityParsing(xml_parser, intcode);

  TRACE_EXIT();
  return SCM_MAKINUM(res);
}
#undef FUNC_NAME

static struct ErrorCodeInfo {
  int expat_code;
  char* symbol;
} error_code_info[] =
{
  { XML_ERROR_NONE, "expat:XML_ERROR_NONE" },
  { XML_ERROR_NO_MEMORY, "expat:XML_ERROR_NO_MEMORY" },
  { XML_ERROR_SYNTAX, "expat:XML_ERROR_SYNTAX" },
  { XML_ERROR_NO_ELEMENTS, "expat:XML_ERROR_NO_ELEMENTS" },
  { XML_ERROR_INVALID_TOKEN, "expat:XML_ERROR_INVALID_TOKEN" },
  { XML_ERROR_UNCLOSED_TOKEN, "expat:XML_ERROR_UNCLOSED_TOKEN" },
  { XML_ERROR_PARTIAL_CHAR, "expat:XML_ERROR_PARTIAL_CHAR" },
  { XML_ERROR_TAG_MISMATCH, "expat:XML_ERROR_TAG_MISMATCH" },
  { XML_ERROR_DUPLICATE_ATTRIBUTE,
    "expat:XML_ERROR_DUPLICATE_ATTRIBUTE" },
  { XML_ERROR_JUNK_AFTER_DOC_ELEMENT,
    "expat:XML_ERROR_JUNK_AFTER_DOC_ELEMENT" },
  { XML_ERROR_PARAM_ENTITY_REF, "expat:XML_ERROR_PARAM_ENTITY_REF" },
  { XML_ERROR_UNDEFINED_ENTITY, "expat:XML_ERROR_UNDEFINED_ENTITY" },
  { XML_ERROR_RECURSIVE_ENTITY_REF,
    "expat:XML_ERROR_RECURSIVE_ENTITY_REF" },
  { XML_ERROR_ASYNC_ENTITY, "expat:XML_ERROR_ASYNC_ENTITY" },
  { XML_ERROR_BAD_CHAR_REF, "expat:XML_ERROR_BAD_CHAR_REF" },
  { XML_ERROR_BINARY_ENTITY_REF, "expat:XML_ERROR_BINARY_ENTITY_REF" },
  { XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF,
    "expat:XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF" },
  { XML_ERROR_MISPLACED_XML_PI, "expat:XML_ERROR_MISPLACED_XML_PI" },
  { XML_ERROR_UNKNOWN_ENCODING, "expat:XML_ERROR_UNKNOWN_ENCODING" },
  { XML_ERROR_INCORRECT_ENCODING, "expat:XML_ERROR_INCORRECT_ENCODING" },
  { XML_ERROR_UNCLOSED_CDATA_SECTION,
    "expat:XML_ERROR_UNCLOSED_CDATA_SECTION" },
  { XML_ERROR_EXTERNAL_ENTITY_HANDLING,
    "expat:XML_ERROR_EXTERNAL_ENTITY_HANDLING" },
  { XML_ERROR_NOT_STANDALONE, "expat:XML_ERROR_NOT_STANDALONE" } };

static int 
error_codes_count = sizeof(error_code_info) / (sizeof(int) + sizeof(char*));

/*
 * XML_Error
 */
SCM
error_int_to_symbol(int code)
{
  int i;
  
  for(i = 0; i < error_codes_count; i++) {
    if(error_code_info[i].expat_code == code)
      return (SCM) gh_symbol2scm(error_code_info[i].symbol);
  }

  return SCM_BOOL_F;
}

int
error_symbol_to_int(SCM code)
{
  char* symbol;
  int i;

  symbol = (char*)gh_symbol2newstr(code, NULL);

  for(i = 0; i < error_codes_count; i++) {
    if(strcmp(error_code_info[i].symbol, symbol) == 0) {
      free(symbol);
      return error_code_info[i].expat_code;
    }
  }

  free(symbol);
  return -1;
}

/*
 * Not part of the expat API, but may be convenient.
 * Returns the Scheme symbol which corresponds to an Expat integer error code.
 */
SCM_DEFINE(mixp_error_code, "mixp:expat-error-code", \
           1, 0, 0, (SCM code), "")
#define FUNC_NAME mixp_error_code
{
  SCM_ASSERT(SCM_INUMP(code) && SCM_INUM(code) <= 22, code, SCM_ARG1, 
	     "mixp:expat-error-code");
  return error_int_to_symbol(SCM_INUM(code));
}
#undef FUNC_NAME

/*
 * xml_get_error_code / expat:get-error-code / XML_GetErrorCode
 * Returns a symbol corresponding to the error code
 */
SCM_DEFINE(xml_get_error_code, "expat:get-error-code", \
           1, 0, 0, (SCM parser), "")
#define FUNC_NAME xml_get_error_code
{
  XML_Parser xml_parser;
  init_xml_parser0(parser, "expat:get-error-code", &xml_parser);

  return error_int_to_symbol(XML_GetErrorCode(xml_parser));
}
#undef FUNC_NAME

/*
 * expat:get-current-byte-count /
 * xml_get_current_byte_count / XML_GetCurrentByteCount
 */
SCM_DEFINE(xml_get_current_byte_count, "expat:get-current-byte-count", \
           1, 0, 0, (SCM parser), "")
#define FUNC_NAME xml_get_current_byte_count
{
  TRACE_ENTER("xml_get_current_byte_count");
  XML_Parser xml_parser;
  int res;

  init_xml_parser0(parser, "expat:get-current-byte-count",
		   &xml_parser);

  res = XML_GetCurrentByteCount(xml_parser);

  TRACE_EXIT();
  return SCM_MAKINUM(res);
}
#undef FUNC_NAME

/*
 * expat:get-current-line-number /xml_get_current_line_number /
 * XML_GetCurrentLineNumber 
 */
SCM_DEFINE(xml_get_current_line_number, "expat:get-current-line-number", \
           1, 0, 0, (SCM parser), "")
#define FUNC_NAME xml_get_current_line_number
{
  SCM_ASSERT(XML_PARSERP(parser), parser, SCM_ARG1, 
	     "expat:get-error-code");
  
  return SCM_MAKINUM(XML_GetCurrentLineNumber(SMOB_TO_XML_PARSER(parser)));
}
#undef FUNC_NAME

/*
 * expat:get-current-column-number /xml_get_current_column_number /
 * XML_GetCurrentColumnNumber 
 */
SCM_DEFINE(xml_get_current_column_number, "expat:get-current-column-number", \
           1, 0, 0, (SCM parser), "")
#define FUNC_NAME xml_get_current_column_number
{
  SCM_ASSERT(XML_PARSERP(parser), parser, SCM_ARG1,
	     "expat:get-error-code");
  
  return SCM_MAKINUM(XML_GetCurrentColumnNumber(SMOB_TO_XML_PARSER(parser)));
}
#undef FUNC_NAME

/*
 * expat:get-current-byte-index /xml_get_current_byte_index /
 * XML_GetCurrentByteIndex 
 *
 * TBD : XML_GetCurrentByteIndex returns a long, I should check that
 * it is not too big for a SCM (?)
 */
SCM_DEFINE(xml_get_current_byte_index, "expat:get-current-byte-index", \
           1, 0, 0, (SCM parser), "")
#define FUNC_NAME xml_get_current_byte_index
{
  SCM_ASSERT(XML_PARSERP(parser), parser, SCM_ARG1, 
	     "expat:get-error-code");
  
  return SCM_MAKINUM(XML_GetCurrentByteIndex(SMOB_TO_XML_PARSER(parser)));
}
#undef FUNC_NAME

/* do not expose this API, the garbage collector takes care of destroying
   the parser */
/*= /*
 *=  * expat:parser-free / xml_parser_free /XML_ParserFree
 *=  *//*= 
 *= SCM
 *= xml_parser_free(SCM parser)
 *= {
 *=   TRACE_ENTER("xml_parser_free");
 *=   XML_Parser xml_parser;
 *= 
 *=   init_xml_parser0(parser, "expat:parser-free",
 *= 		   &xml_parser);
 *= 
 *=   ensure_xml_parser_freed(parser);
 *= 
 *=   TRACE_EXIT();
 *=   return SCM_UNSPECIFIED;
 *= } =*/
/* */

/*
 * expat:error-string / xml_error_string / XML_ErrorString
 * code is a Scheme symbol.
 */
SCM_DEFINE(xml_error_string, "expat:error-string", \
           1, 0, 0, (SCM code), "")
#define FUNC_NAME xml_error_string
{
  SCM_ASSERT(SCM_NIMP(code) && SCM_SYMBOLP(code), code, SCM_ARG1,
	     "expat:error-string");

  return scm_makfrom0str(XML_ErrorString(error_symbol_to_int(code)));
}
#undef FUNC_NAME

/**********************************************************************
 *
 * END OF THE EXPAT INTERFACE
 *
 **********************************************************************/

/*
 * Initialize this module. This function must be called from a C program
 * before using this module.
 */ 

void
init_gexpat()
{ 

  /* Create the Scheme procedures */

  /*
  gh_new_procedure("expat:parser-create", 
		   xml_parser_create, 0, 1, 0); 

  gh_new_procedure("expat:parser-create-ns", 
		   xml_parser_create_ns, 0, 2, 0); 
  
  gh_new_procedure("expat:set-element-handler",
		   xml_set_element_handler, 3, 0, 0);

  gh_new_procedure("expat:set-character-data-handler",
		   xml_set_character_data_handler, 2, 0, 0);

  gh_new_procedure("expat:set-processing-instruction-handler",
		   xml_set_processing_instruction_handler, 2, 0, 0);

  gh_new_procedure("expat:set-comment-handler",
		   xml_set_comment_handler, 2, 0, 0);

  gh_new_procedure("expat:set-cdata-section-handler",
		   xml_set_cdata_section_handler, 3, 0, 0);

  gh_new_procedure("expat:set-default-handler",
		   xml_set_default_handler, 2, 0, 0);

  gh_new_procedure("expat:set-default-handler-expand",
		   xml_set_default_handler_expand, 2, 0, 0);

  gh_new_procedure("expat:set-unparsed-entity-decl-handler",
		   xml_set_unparsed_entity_decl_handler, 2, 0, 0);

  gh_new_procedure("expat:set-notation-decl-handler",
		   xml_set_notation_decl_handler, 2, 0, 0);

  gh_new_procedure("expat:set-namespace-decl-handler",
		   xml_set_namespace_decl_handler, 3, 0, 0);

  gh_new_procedure("expat:set-not-standalone-handler",
		   xml_set_not_standalone_handler, 2, 0, 0);

  gh_new_procedure("expat:set-external-entity-ref-handler",
		   xml_set_external_entity_ref_handler, 2, 0, 0);

  gh_new_procedure("expat:set-external-entity-ref-handler-arg",
		   xml_set_external_entity_ref_handler_arg, 3, 0, 0);

  gh_new_procedure("expat:set-unknown-encoding-handler",
		   xml_set_unknown_encoding_handler, 3, 0, 0);

  gh_new_procedure("expat:default-current",
		   xml_default_current,1, 0, 0);

  gh_new_procedure("expat:set-user-data",
		   xml_set_user_data, 2, 0, 0);

  gh_new_procedure("expat:get-user-data",
		   xml_get_user_data, 1, 0, 0);

  gh_new_procedure("expat:set-encoding",
		   xml_set_encoding, 2, 0, 0);

  gh_new_procedure("expat:use-parser-as-handler-arg",
		   xml_use_parser_as_handler_arg, 1, 0, 0);

  gh_new_procedure("expat:set-base",
		   xml_set_base, 2, 0, 0);

  gh_new_procedure("expat:get-base",
		   xml_get_base, 1, 0, 0);

  gh_new_procedure("expat:get-specified-attribute-count",
		   xml_get_specified_attribute_count, 1, 0, 0);

  gh_new_procedure("expat:parse",
		   xml_parse, 3, 0, 0);

  gh_new_procedure("expat:parse-buffer",
		   xml_parse_buffer,2, 1, 0);

  gh_new_procedure("expat:set-param-entity-parsing",
		   xml_set_param_entity_parsing, 2, 0, 0);

  gh_new_procedure("expat:get-error-code",
		   xml_get_error_code, 1, 0, 0);
  
  gh_new_procedure("expat:get-current-byte-count", 
		   xml_get_current_byte_count, 1, 0, 0);

  gh_new_procedure("expat:get-current-line-number",
		   xml_get_current_line_number, 1, 0, 0);

  gh_new_procedure("expat:get-current-column-number",
		   xml_get_current_column_number, 1, 0, 0);

  gh_new_procedure("expat:get-current-byte-index",
		   xml_get_current_byte_index, 1, 0, 0);
   */

/*=   gh_new_procedure("expat:parser-free",
 *= 		   xml_parser_free, 1, 0, 0); =*/

  /*
  gh_new_procedure("expat:error-string",
		   xml_error_string, 1, 0, 0);
   */

  /* Additional functions */
  
  /*
  gh_new_procedure("mixp:expat-error-code",
		   mixp_error_code, 1, 0, 0);
   */

  /*  Our internal data structures */
  /*
  gh_new_procedure("mixp:parser?",
		   xml_parser_p, 1, 0, 0);
  
  init_xml_parser_type();
  init_xml_encoding_type();
   */

  init_xml_parser_type();
  init_xml_encoding_type();

#ifndef SCM_MAGIC_SNARFER
# ifndef MKDEP
#  include "gexpat.x"
# endif /* MKDEP */
#endif /* SCM_MAGIC_SNARFER */

}

/*
 * "module init" function for this module (see ice-9/boot-9.scm: 
 *  Dynamic linking of modules)
 *  Note that the shared library must be foo/libbar.so if the
 *  Guile module name is foo:bar.
 */
/*
void
scm_init_xml_expat_module()
{
  scm_register_module_xxx(GEXPAT_MODULE_NAME, init_gexpat);
}
 */

void scm_init_xml_expat()
{
  init_gexpat();
}
