/*
 * The Original Code is Mixp.
 *
 * The Initial Developer of the Original Code is Thierry Bézecourt.
 * Portions created by Thierry Bézecourt are Copyright (C) 1999, 2000
 * Thierry Bézecourt. All Rights Reserved.
 * 
 * Copyright (C) 2002 Dmitry Morozhnikov <dmiceman@mail.ru>
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

/* This file defines the functions which are passed to expat as
   handlers. This functions are generic because, whatever Scheme
   function was specified by the user, we will use the same handler
   function, and the Scheme handler is hidden in the user data */

#define DATA_OR_PARSER(ud) \
   ((ud)->use_parser_as_handler_arg ? (ud)->parser : (ud)->data)

#if DEBUG

# define TRACE(msg) printf(msg); printf("\n")
# define TRACE1(fmt, w1) printf(fmt, w1); printf("\n")
# define TRACE2(fmt, w1, w2) printf(fmt, w1, w2); printf("\n")
# define TRACE3(fmt, w1, w2, w3) printf(fmt, w1, w2, w3); printf("\n")
# define TRACE4(fmt, w1, w2, w3, w4) printf(fmt, w1, w2, w3, w4); printf("\n")
# define TRACE5(fmt, w1, w2, w3, w4, w5) \
           printf(fmt, w1, w2, w3, w4, w5); printf("\n")
# define TRACE6(fmt, w1, w2, w3, w4, w5, w6) \
           printf(fmt, w1, w2, w3, w4, w5, w6); printf("\n")

# define TRACE_ENTER(function) \
           char* __function_name__ = function; \
           int __nothing__ = printf ("* %s: enter\n", function)
# define TRACE_EXIT() printf ("* %s: exit\n", __function_name__)

#else

# define TRACE(msg)
# define TRACE1(fmt, w1)
# define TRACE2(fmt, w1, w2)
# define TRACE3(fmt, w1, w2, w3)
# define TRACE4(fmt, w1, w2, w3, w4)
# define TRACE5(fmt, w1, w2, w3, w4, w5)
# define TRACE6(fmt, w1, w2, w3, w4, w5, w6)

# define TRACE_ENTER(function) int __nothing__
# define TRACE_EXIT()

#endif

/**********************************************************************
 *
 * The real Expat handlers
 *
 **********************************************************************/
void
generic_start_handler(void *data,		      
		      const XML_Char *name, 
		      const XML_Char **atts)
{
  TRACE_ENTER("generic_start_handler");

  user_data *ud = (user_data *)data;
  SCM handler = ud->start_element_handler;

  /* Note: the start handler is unbound if #f was specified as the
     second argument of xml:expat:set-element-handler */
  if(! SCM_UNBNDP(handler)) {

    int nb = 0;
    SCM scm_atts = SCM_EOL;

    for(; atts[0] != NULL && atts[1] != NULL; atts += 2)
      scm_atts = scm_cons(scm_cons(scm_makfrom0str(atts[0]),
				   scm_makfrom0str(atts[1])),
			  scm_atts);

    gh_call3(handler, DATA_OR_PARSER(ud), scm_makfrom0str(name) , scm_atts);
  }

  TRACE_EXIT();
}

void 
generic_end_handler(void *data,
		    const XML_Char *name)
{
  TRACE_ENTER("generic_end_handler");

  user_data *ud = (user_data *)data;
  SCM handler = ud->end_element_handler;

  if(! SCM_UNBNDP(handler)) {
    gh_call2(handler, DATA_OR_PARSER(ud), scm_makfrom0str(name));
  }

  TRACE_EXIT();
}

void 
generic_character_data_handler(void *data,
			       const XML_Char *name,
			       int len)
{
  TRACE_ENTER("generic_character_data_handler");

  user_data *ud = (user_data *)data;

  gh_call2(ud->character_data_handler, 
	   DATA_OR_PARSER(ud),
	   scm_mem2string(name, len));

  TRACE_EXIT();
}

void
generic_processing_instruction_handler(void *data,
				       const XML_Char* target,
				       const XML_Char* pi_data)
{
  user_data *ud = (user_data *)data;

  gh_call3(ud->processing_instruction_handler, 
	   DATA_OR_PARSER(ud),
	   scm_makfrom0str(target), 
	   scm_makfrom0str(pi_data));
}

void
generic_comment_handler(void *data,
			const XML_Char* comment_data)
{
  user_data *ud = (user_data *)data;
  gh_call2(ud->comment_handler,
	   DATA_OR_PARSER(ud),
	   scm_makfrom0str(comment_data));
}

void
generic_start_cdata_section_handler(void *data)
{
  TRACE_ENTER("generic_start_cdata_section_handler");

  user_data *ud = (user_data *)data;
  SCM handler = ud->start_cdata_section_handler;

  if(! SCM_UNBNDP(handler))
    gh_call1(handler, DATA_OR_PARSER(ud));

  TRACE_EXIT();
}

void
generic_end_cdata_section_handler(void *data)
{
  user_data *ud = (user_data *)data;
  SCM handler = ud->end_cdata_section_handler;

  if(! SCM_UNBNDP(handler)) {
    gh_call1(handler, DATA_OR_PARSER(ud));
  }
}

void
generic_default_handler(void *data,
			const XML_Char* s,
			int len)
{
  user_data *ud = (user_data *)data;
  gh_call2(ud->default_handler, DATA_OR_PARSER(ud),
	   scm_mem2string(s, len));
}

void
generic_default_handler_expand(void *data,
			       const XML_Char* s,
			       int len)
{
  user_data *ud = (user_data *)data;
  gh_call2(ud->default_handler_expand, DATA_OR_PARSER(ud),
	   scm_mem2string(s, len));
}

void
generic_unparsed_entity_decl_handler(void *data,
				     const XML_Char* entityName,
				     const XML_Char* base,
				     const XML_Char* systemId,
				     const XML_Char* publicId,
				     const XML_Char* notationName)
{
  user_data *ud = (user_data *)data;

  SCM arg1 = scm_makfrom0str(entityName); 
  SCM arg2 = scm_makfrom0str(base); 
  SCM arg3 = scm_makfrom0str(systemId); 
  SCM arg4 = scm_makfrom0str(publicId); 
  SCM arg5 = scm_makfrom0str(notationName);
    
  scm_apply(ud->unparsed_entity_decl_handler,
	    DATA_OR_PARSER(ud),
	    scm_cons2(arg1, arg2,
		      scm_cons2(arg3, arg4,
				scm_cons(arg5, scm_listofnull))));
}

void
generic_notation_decl_handler(void *data,
			      const XML_Char* notationName,
			      const XML_Char* base,
			      const XML_Char* systemId,
			      const XML_Char* publicId)
{
  TRACE_ENTER("generic_notation_decl_handler");
  user_data *ud = (user_data *)data;

  SCM arg1 = scm_makfrom0str(notationName);
  SCM arg2 = base ? scm_makfrom0str(base) : SCM_BOOL_F;
  SCM arg3 = systemId ? scm_makfrom0str(systemId) : SCM_BOOL_F; 
  SCM arg4 = publicId ? scm_makfrom0str(publicId) : SCM_BOOL_F;

  scm_apply(ud->notation_decl_handler,
	    DATA_OR_PARSER(ud),
	    scm_cons2(arg1, arg2,
		      scm_cons2(arg3, arg4, scm_listofnull)));

  TRACE_EXIT();
}

void
generic_start_namespace_decl_handler(void *data,
				     const XML_Char* prefix,
				     const XML_Char* uri)
{
  user_data *ud = (user_data *)data;
  SCM handler = ud->start_namespace_decl_handler;

  if(! SCM_UNBNDP(handler)) {
    gh_call3(handler, DATA_OR_PARSER(ud),
	     scm_makfrom0str(prefix),
	     scm_makfrom0str(uri));
  }
}

void
generic_end_namespace_decl_handler(void *data,
				   const XML_Char* prefix)
{
  user_data *ud = (user_data *)data;
  SCM handler = ud->end_namespace_decl_handler;

  if(! SCM_UNBNDP(handler)) {
    gh_call2(handler, DATA_OR_PARSER(ud),
	     scm_makfrom0str(prefix));
  }
}

int
generic_not_standalone_handler(void *data)
{
  int res = 1; /* if 0, then the parser will stop */

  user_data *ud = (user_data *)data;

  res = SCM_INUM(gh_call1(ud->not_standalone_handler, DATA_OR_PARSER(ud)));

  return res;
}

/*
 * Note: since XML_SetExternalEntityRefHandlerArg() is never called,
 * the first argument will be necessarily a XML_Parser.  The handler
 * should return 0 if processing should not continue because of a
 * fatal error in the handling of the external entity.  In this case
 * the calling parser will return an
 * XML_ERROR_EXTERNAL_ENTITY_HANDLING
 */
int
generic_external_entity_ref_handler(XML_Parser xml_parser,
				    const XML_Char *context,
				    const XML_Char *base,
				    const XML_Char *systemId,
				    const XML_Char *publicId)
{
  int res = 1;
  scm_t_port *port;
  SCM s_port;
  user_data* ud = (user_data *)XML_GetUserData(xml_parser);
  XML_Parser ext_xml_parser;
  short end_parsing = 0;
  scm_t_ptob_descriptor *ptob;
  
  SCM arg0 = ((ud->external_entity_ref_handler_arg != SCM_UNDEFINED) ?
	      ud->external_entity_ref_handler_arg : ud->parser);
  SCM arg1 = context ? scm_makfrom0str(context) : SCM_BOOL_F;
  SCM arg2 = base ? scm_makfrom0str(base) : SCM_BOOL_F;
  SCM arg3 = scm_makfrom0str(systemId);
  SCM arg4 = publicId ? scm_makfrom0str(publicId) : SCM_BOOL_F;
  SCM handler = ud->external_entity_ref_handler;

  /*return 1;*/
  s_port = scm_apply(handler, arg0, 
		     scm_cons2(arg1, arg2, 
			       scm_cons2(arg3, arg4, 
					 scm_listofnull)));
  SCM_ASSERT(SCM_NIMP(s_port) && SCM_OPINPORTP(s_port), 
	     s_port, SCM_ARGn, 
	     "external-entity-ref handler");
  
  port = SCM_PTAB_ENTRY(s_port);

  ext_xml_parser =
    XML_ExternalEntityParserCreate(xml_parser, context, NULL);

  while(! end_parsing) {
    int parse_res;

    /* scm_read_line returns a cons of the line and the termination
       character(s?)*/
    SCM lineinfo = scm_read_line(s_port);
    SCM line = SCM_CAR(lineinfo);
    SCM term = SCM_CDR(lineinfo);
    
    char* str_line = (line == SCM_EOF_VAL) ? "" : SCM_STRING_CHARS(line);
    int len_line = strlen(str_line);
    char* complete_line;

    if(term == SCM_EOF_VAL) {
      complete_line = (char*) malloc(len_line + 1); 
      strcpy(complete_line, str_line);
      complete_line[len_line] = '\0';
    }
    else {
      len_line++;
      complete_line = (char*) malloc(len_line + 1); 
      strcpy(complete_line, str_line);
      complete_line[len_line-1] = SCM_CHAR(term);
      complete_line[len_line] = '\0';
    }

    if(term == SCM_EOF_VAL)
      end_parsing = 1;

    parse_res = XML_Parse(ext_xml_parser, complete_line, len_line,
		    end_parsing);

    if(parse_res == 0) {
      enum XML_Error code = XML_GetErrorCode(ext_xml_parser);
      int nb_line = XML_GetCurrentLineNumber(ext_xml_parser);
      int nb_column = XML_GetCurrentColumnNumber(ext_xml_parser);
      fprintf(stderr,
	      "Parsing error in external entity %s at line %d, column %d: %s\n",
	      systemId, nb_line, nb_column, XML_ErrorString(code));
      /* Return 0 from the handler if the external entity is not
	 well-formed */
      res = 0;
      end_parsing = 1;
    }
  }

  /* Apparently, I must not close the port because the garbage
     collector closes it again.
  ptob = scm_ptobs + SCM_PTOBNUM(s_port);
  ptob->close(s_port);
  */

  XML_ParserFree(ext_xml_parser);

  return res;
}

/*
 * TBD: support of encodingHandlerData
 * The Scheme handler must return either SCM_BOOL_F if it cannot provide
 * information about the encoding, or return an encoding smob created
 * with make-xml-encoding.
 */
int
generic_unknown_encoding_handler(void *encodingHandlerData,
				 const XML_Char *name,
				 XML_Encoding *info)
{
  TRACE_ENTER("generic_unknown_encoding_handler");
  int res = 0; /* 1 means that the handler fills the XML_Encoding 
		* structure */
  
  encoding_handler_data *ehd = (encoding_handler_data *)encodingHandlerData;

  SCM arg1 = scm_makfrom0str(name);
  SCM encoding_info;
  
  encoding_info = (SCM) gh_call2(ehd->handler, ehd->data,
                                 scm_makfrom0str(name));
  
  /* TBD: type checking of encoding_enfo */
  
  if(SCM_NFALSEP(encoding_info)) {
    XML_Encoding *tmp = xml_encoding_smob_to_expat(encoding_info);
    TRACE("Encoding recognized by the handler");
    *info = *tmp;
    res = 1;
  }
  else {
    TRACE("Encoding not recognized by the handler");
    res = 0;
  }

  TRACE1("reponse: %d.\n", res);
  TRACE_EXIT();
  return res;
}
