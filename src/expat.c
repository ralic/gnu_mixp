/* expat.c --- implement (mixp expat)

   Copyright (C) 2007, 2009, 2010, 2011 Thien-Thi Nguyen
   Portions Copyright (C) 1999, 2000 Thierry Bézecourt

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

/* Author: Thierry Bézecourt  */

#include "config.h"

#include <expat.h>
/* Regularity is nice.  */
#define XML_SetNiceDefaultExpandHandler \
  XML_SetDefaultHandlerExpand
#define XML_SetNiceUnknownEncodingHandler(p,h) \
  XML_SetUnknownEncodingHandler (p, h, ud)

#include "gi.h"

#define STRMAYBE(x)  DEFAULT_FALSE (x, STRING (x))

#define SPECIFIEDP(x)  (SCM_UNSPECIFIED != (x))

#define UNBOUND_MEANS_UNSPECIFIED(x) \
  if (! GIVENP (x)) x = SCM_UNSPECIFIED

#define SPECIFIED_NOT_FALSE(x)  (SPECIFIEDP (x) && NOT_FALSEP (x))

#define PROCP(x)    (SCM_NIMP (x) && SCM_CLOSUREP (x))


struct enumsym
{
  char *nm;
  SCM   sy;
};

#define PAIR(x)  { #x, SCM_UNSPECIFIED }

static const char const *
hnames[] =
{
  "element-start",
  "element-end",
  "character-data",
  "processing-instruction",
  "comment",
  "cdata-section-start",
  "cdata-section-end",
  "lastchoice",
  "lastchoice-expand",
  "unparsed-entity-decl",
  "notation-decl",
  "namespace-decl-start",
  "namespace-decl-end",
  "not-standalone",
  "external-entity-ref",
  "unknown-encoding"
};

static size_t
n_hnames = sizeof (hnames) / sizeof (char *);

static SCM halist;

enum hindices
  {
    element_start,
    element_end,
    character_data,
    processing_instruction,
    comment,
    cdata_section_start,
    cdata_section_end,
    lastchoice,
    lastchoice_expand,
    unparsed_entity_decl,
    notation_decl,
    namespace_decl_start,
    namespace_decl_end,
    not_standalone,
    external_entity_ref,
    unknown_encoding,
    hindex_count
  };

#define get_ud(x)  ((SCM *) XML_GetUserData (x))
#define as_ud(x)   ((SCM *) (x))

#define udsel(x,h)  (as_ud (x) [h])

/* Smob for XML_Parser.  */

static long parser_tag;

#define PARSERP(smob) \
  SCM_SMOB_PREDICATE (parser_tag, smob)
#define UNPACK_PARSER(smob) \
  ((XML_Parser) SMOBDATA (smob))

#define VALIDATE_PARSER()                                       \
  do {                                                          \
    ASSERT (parser, PARSERP (parser), SCM_ARG1);                \
    p = UNPACK_PARSER (parser);                                 \
  } while (0)

#define VALIDATE_PARSER_HANDLER()                               \
  do {                                                          \
    VALIDATE_PARSER ();                                         \
    ASSERT (handler, PROCP (handler), SCM_ARG2);                \
  } while (0)

#define VALIDATE_PARSER_START_END()                             \
  do {                                                          \
    VALIDATE_PARSER ();                                         \
    ASSERT (start, ! NOT_FALSEP (start) || PROCP (start),       \
            SCM_ARG2);                                          \
    ASSERT (end, ! NOT_FALSEP (end) || PROCP (end),             \
            SCM_ARG3);                                          \
  } while (0)

static SCM
mark_parser (SCM obj)
{
  XML_Parser p = UNPACK_PARSER (obj);
  int i; SCM *h;

  for (i = 0, h = get_ud (p); i < hindex_count; i++, h++)
    if (SPECIFIEDP (*h))
      scm_gc_mark (*h);
  RETURN_FALSE ();
}

#define UD_SIZE  (hindex_count * sizeof (SCM))

static size_t
free_parser (SCM obj)
{
  XML_Parser p = UNPACK_PARSER (obj);

  free (get_ud (p));
  XML_SetUserData (p, NULL);
  XML_ParserFree (p);
  SCM_SET_SMOB_DATA (obj, (p = NULL));

  return UD_SIZE;
}

static int
print_parser (SCM obj, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<XML-Parser 0x", port);
  scm_intprint ((long int) UNPACK_PARSER (obj), 16, port);
  scm_putc ('>', port);
  return 1;
}

static SCM
make_parser (XML_Parser p)
{
  int i; SCM *ud, *h;

  SCM_DEFER_INTS;
  ud = as_ud (malloc (UD_SIZE));
  for (i = 0, h = ud; i < hindex_count; i++, h++)
    *h = SCM_UNSPECIFIED;
  XML_SetUserData (p, ud);
  SCM_ALLOW_INTS;

  SCM_RETURN_NEWSMOB (parser_tag, p);
}

/* Smob for XML_Encoding.  */

static long encoding_tag;

#define ENCODINGP(smob) \
  SCM_SMOB_PREDICATE (encoding_tag, smob)
#define UNPACK_ENCODING(smob) \
  ((XML_Encoding *) SMOBDATA (smob))

/* A structure used to wrap the data element of a XML_Encoding structure.  */
typedef struct
{
  /* A reference on the map in the XML_Encoding object, here only because
     generic_encoding_convert needs it.  Do not free it, it's really an
     alias for the map in the XML_Encoding object.  */
  int *map;

  /* A conversion function for multi-byte sequences.  */
  SCM convert;

  /* A release function called when the encoding is not used any more.  */
  SCM release;

} encoding_data;

#define get_ed(x)  ((encoding_data *) x->data)

static SCM
mark_encoding (SCM obj)
{
  encoding_data *xed = get_ed (UNPACK_ENCODING (obj));

  scm_gc_mark (xed->convert);

  return xed->release;
}

static size_t
free_encoding (SCM obj)
{
  XML_Encoding *enc = UNPACK_ENCODING (obj);

  free (enc->data);
  free (enc);

  return sizeof (XML_Encoding) + sizeof (encoding_data);
}

static int
print_encoding (SCM obj, SCM port, scm_print_state *pstate)
{
  XML_Encoding *enc = UNPACK_ENCODING (obj);
  int i;

  scm_puts ("#<XML-Encoding", port);
  for (i = 0; i < 256; i++)
    {
      scm_puts (" ", port);
      scm_display (SCM_MAKINUM (enc->map[i]), port);
    }
  scm_putc ('>', port);
  return 1;
}

static int
generic_encoding_convert (void *data, const char *s)
{
  encoding_data *xed = (encoding_data *) data;

  return C_INT
    (CALL1 (xed->convert, BSTRING (s, xed->map[(unsigned char) *s])));
}

static void
generic_encoding_release (void *data)
{
  encoding_data *xed = (encoding_data *) data;

  CALL0 (xed->release);
}


/* "Generic" handlers: whatever Scheme function was specified by the
   user, Expat sees the same handler function, and the Scheme handler
   is hidden in the user data.  */

static void
generic_element_start (void *data, const XML_Char *name,
                       const XML_Char ** atts)
{
  SCM handler = udsel (data, element_start);

  /* Note: the start handler is unbound if #f was specified as the
     second argument of set-element-handler.  */
  if (SPECIFIEDP (handler))
    {
      SCM alist = SCM_EOL;

      for (; atts[0] && atts[1]; atts += 2)
        alist = scm_acons (STRING (atts[0]),
                           STRING (atts[1]),
                           alist);

      CALL2 (handler, STRING (name), alist);
    }
}

static void
generic_element_end (void *data, const XML_Char *name)
{
  SCM handler = udsel (data, element_end);

  if (SPECIFIEDP (handler))
    CALL1 (handler, STRING (name));
}

static void
generic_character_data (void *data, const XML_Char *name, int len)
{
  SCM handler = udsel (data, character_data);

  if (SPECIFIEDP (handler))
    CALL1 (handler, BSTRING (name, len));
}

static void
generic_processing_instruction (void *data,
                                const XML_Char *target,
                                const XML_Char *pi_data)
{
  SCM handler = udsel (data, processing_instruction);

  if (SPECIFIEDP (handler))
    CALL2 (handler, STRING (target), STRING (pi_data));
}

static void
generic_comment (void *data, const XML_Char *comment_data)
{
  SCM handler = udsel (data, comment);

  if (SPECIFIEDP (handler))
    CALL1 (handler, STRING (comment_data));
}

static void
generic_cdata_section_start (void *data)
{
  SCM handler = udsel (data, cdata_section_start);

  if (SPECIFIEDP (handler))
    CALL0 (handler);
}

static void
generic_cdata_section_end (void *data)
{
  SCM handler = udsel (data, cdata_section_end);

  if (SPECIFIEDP (handler))
    CALL0 (handler);
}

static void
generic_lastchoice (void *data, const XML_Char *s, int len)
{
  SCM handler = udsel (data, lastchoice);

  if (SPECIFIEDP (handler))
    CALL1 (handler, BSTRING (s, len));
}

static void
generic_lastchoice_expand (void *data, const XML_Char *s, int len)
{
  SCM handler = udsel (data, lastchoice_expand);

  if (SPECIFIEDP (handler))
    CALL1 (handler, BSTRING (s, len));
}

static void
generic_unparsed_entity_decl (void *data,
                              const XML_Char *entityName,
                              const XML_Char *base,
                              const XML_Char *systemId,
                              const XML_Char *publicId,
                              const XML_Char *notationName)
{
  SCM handler = udsel (data, unparsed_entity_decl);
  SCM arg1 = STRMAYBE (entityName);
  SCM arg2 = STRMAYBE (base);
  SCM arg3 = STRMAYBE (systemId);
  SCM arg4 = STRMAYBE (publicId);
  SCM arg5 = STRMAYBE (notationName);

  APPLY (handler, LISTIFY (arg1, arg2, arg3, arg4, arg5,
                           SCM_UNDEFINED));
}

static void
generic_notation_decl (void *data,
                       const XML_Char *notationName,
                       const XML_Char *base,
                       const XML_Char *systemId,
                       const XML_Char *publicId)
{
  SCM handler = udsel (data, notation_decl);
  SCM arg1 = STRING (notationName);
  SCM arg2 = STRMAYBE (base);
  SCM arg3 = STRMAYBE (systemId);
  SCM arg4 = STRMAYBE (publicId);

  APPLY (handler, LISTIFY (arg1, arg2, arg3, arg4,
                           SCM_UNDEFINED));
}

static void
generic_namespace_decl_start (void *data,
                              const XML_Char *prefix,
                              const XML_Char *uri)
{
  SCM handler = udsel (data, namespace_decl_start);

  if (SPECIFIEDP (handler))
    CALL2 (handler, STRING (prefix), STRING (uri));
}

static void
generic_namespace_decl_end (void *data, const XML_Char *prefix)
{
  SCM handler = udsel (data, namespace_decl_end);

  if (SPECIFIEDP (handler))
    CALL1 (handler, STRING (prefix));
}

static int
generic_not_standalone (void *data)
{
  SCM handler = udsel (data, not_standalone);

  return (SPECIFIEDP (handler)
          ? SCM_INUM (CALL0 (handler))
          : 1);
}

static int
generic_external_entity_ref (XML_Parser p,
                             const XML_Char *context,
                             const XML_Char *base,
                             const XML_Char *systemId,
                             const XML_Char *publicId)
{
  XML_Parser ext_p;
  SCM port, nl, buf;
  int rv = 1, donep = 0;

  port = APPLY (udsel (get_ud (p), external_entity_ref),
                LISTIFY (STRMAYBE (context),
                         STRMAYBE (base),
                         STRMAYBE (systemId),
                         STRMAYBE (publicId),
                         SCM_UNDEFINED));

  SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port),
              port, SCM_ARGn, "external-entity-ref handler");

  ext_p = XML_ExternalEntityParserCreate (p, context, NULL);

#define NEWBUF(size) \
  scm_make_string (SCM_MAKINUM (size), SCM_MAKE_CHAR (0))

  nl = STRING ("\n");
  buf = NEWBUF (1024);

  while (!donep)
    {
      enum XML_Status parse_res;
      size_t nbytes = 0;
      SCM delim, big = SCM_EOL;

#define SHORTP()  (! NOT_FALSEP (delim))
#define EXTRAP()  (! NULLP (big))

      do
        {
          SCM pair = scm_read_delimited_x (nl, buf, SCM_BOOL_T, port,
                                           SCM_UNDEFINED, SCM_UNDEFINED);
          delim   =           CAR (pair);
          nbytes += SCM_INUM (CDR (pair));
          if (SHORTP () || EXTRAP ())
            {
              big = CONS (buf, big);
              buf = NEWBUF (256);
            }
        }
      while (SHORTP ());

      if (EXTRAP ())
        buf = scm_string_append (scm_reverse_x (big, SCM_EOL));

#undef SHORTP
#undef EXTRAP

      parse_res = XML_Parse (ext_p, ROZT (buf), nbytes,
                             (donep = (delim == SCM_EOF_VAL)));

      if (XML_STATUS_OK != parse_res)
        {
          rv = 0;
          donep = 1;
        }
    }

#undef NEWBUF

  XML_ParserFree (ext_p);
  return rv;
}

static int
generic_unknown_encoding (void *data,
                          const XML_Char *name,
                          XML_Encoding *info)
{
  /* 1 means that the handler fills the XML_Encoding structure.  */
  int res = 0;
  SCM handler = udsel (data, unknown_encoding);
  SCM encoding_info = SCM_UNSPECIFIED;

  if (SPECIFIEDP (handler))
    encoding_info = CALL1 (handler, STRING (name));

  if (ENCODINGP (encoding_info))
    {
      XML_Encoding *tmp = UNPACK_ENCODING (encoding_info);
      *info = *tmp;
      res = 1;
    }

  return res;
}


/* Scheme interface.  */

PRIMPROC
(parser_p, "parser?", 1, 0, 0,
 (SCM obj),
 doc: /***********
Return @code{#t} if @var{obj} is an XML-Parser object,
otherwise @code{#f}.  */)
{
  return BOOLEAN (PARSERP (obj));
}

PRIMPROC
(make_encoding, "make-xml-encoding", 3, 0, 0,
 (SCM map, SCM convert, SCM release),
 doc: /***********
Return a new XML-Encoding object.

@var{map} is a vector of length 256.  Each element is an integer
specifying how many bytes are required to decode a multibyte
``character'' whose first byte is that element's index.

@var{convert} is a proc that takes one arg, a unibyte string.
It should return the "Unicode scalar value" of the string,
or -1 if the byte sequence is malformed.

@var{release} is a thunk the parser calls when done
all conversion work.  */)
{
#define FUNC_NAME s_make_encoding
  XML_Encoding *enc = NULL;
  const SCM *map_elts;
  encoding_data *xe_data;
  int i;

  SCM_VALIDATE_VECTOR_LEN (1, map, 256);
  SCM_VALIDATE_CLOSURE (2, convert);
  SCM_VALIDATE_CLOSURE (3, release);

  SCM_DEFER_INTS;
  enc = (XML_Encoding *) scm_must_malloc (sizeof (XML_Encoding),
                                          "XML_Encoding");
  SCM_ALLOW_INTS;

  /* The map goes directly into the XML_Encoding object.  */
  map_elts = SCM_VELTS (map);
  for (i = 0; i < 256; i++)
    enc->map[i] = SCM_INUM (map_elts[i]);

  SCM_DEFER_INTS;
  enc->data = scm_must_malloc (sizeof (encoding_data),
                               "encoding_data");
  SCM_ALLOW_INTS;

  xe_data = get_ed (enc);
  xe_data->map = enc->map;
  xe_data->convert = convert;
  xe_data->release = release;

  enc->convert = generic_encoding_convert;
  enc->release = generic_encoding_release;

  SCM_RETURN_NEWSMOB (encoding_tag, enc);
#undef FUNC_NAME
}

PRIMPROC
(parser_create, "parser-create", 0, 1, 0,
 (SCM encoding),
 doc: /***********
Return a new XML_Parser object.
Optional arg @var{encoding} is a string specifying
the encoding to use (for example, "UTF-8").  */)
{
#define FUNC_NAME s_parser_create
  XML_Char *e = NULL;

  if (GIVENP (encoding))
    SCM_VALIDATE_STRING_COPY (1, encoding, e);

  return make_parser (XML_ParserCreate (e));
#undef FUNC_NAME
}

PRIMPROC
(parser_create_ns, "parser-create-ns", 0, 2, 0,
 (SCM encoding, SCM namespace_separator),
 doc: /***********
Optional arg @var{encoding} is a string specifying
the encoding to use (for example, "UTF-8").
Second optional arg @var{namespace-separator} is a character
used to separate namespaces (for example @code{#\:}).

-sig: ([encoding [namespace-separator]])  */)
{
#define FUNC_NAME s_parser_create_ns
  XML_Char *e = NULL;
  XML_Char c = '\0';

  if (GIVENP (encoding))
    SCM_VALIDATE_STRING_COPY (1, encoding, e);
  if (GIVENP (namespace_separator))
    SCM_VALIDATE_CHAR_COPY (2, namespace_separator, c);

  return make_parser (XML_ParserCreateNS (e, c));
#undef FUNC_NAME
}

PRIMPROC
(default_current, "default-current", 1, 0, 0,
 (SCM parser),
 doc: /***********
Declare that @var{parser} is the default current parser.  */)
{
#define FUNC_NAME s_default_current
  XML_Parser p;

  VALIDATE_PARSER ();

  XML_DefaultCurrent (p);

  RETURN_UNSPECIFIED ();
#undef FUNC_NAME
}

PRIMPROC
(hset_x, "hset!", 1, 0, 1,
 (SCM parser, SCM plist),
 doc: /***********
Set handlers for @var{parser} as specified in @var{plist}.
Valid values in @var{plist} are a procedure or @code{#f} (note,
however, that no arity checks are done on the procedures).
Valid properties in @var{plist} are (TODO: use @@table):

@example
element-start
element-end
character-data
processing-instruction
comment
cdata-section-start
cdata-section-end
lastchoice
lastchoice-expand
unparsed-entity-decl
notation-decl
namespace-decl-start
namespace-decl-end
not-standalone
external-entity-ref
unknown-encoding
@end example  */)
{
#define FUNC_NAME s_hset_x
  XML_Parser p;
  SCM *ud;
  SCM ls, sym, val;

  VALIDATE_PARSER ();
  SCM_VALIDATE_CONS (2, plist);

#define POPTO(recip)  recip = CAR (ls), ls = CDR (ls)

  /* First pass: validate ‘plist’.  */
  ls = plist;
  while (! NULLP (ls))
    {
      POPTO (sym);
      SCM_VALIDATE_SYMBOL (2, sym);
      ASSERT (plist, NOT_FALSEP (scm_assq (sym, halist)), 2);
      SCM_VALIDATE_CONS (2, ls);
      POPTO (val);
      ASSERT (plist, SCM_FALSEP (val) || PROCP (val), 2);
    }

  /* Second pass: do it!  */
  ud = get_ud (p);
  ls = plist;
  while (! NULLP (ls))
    {
      int usep;

      POPTO (sym);
      POPTO (val);

#define SETH(x)                                         \
      usep = NOT_FALSEP (val);                         \
      udsel (ud, x) = usep ? val : SCM_UNSPECIFIED      \

#define M(x)  (usep ? x : NULL)         /* (maybe) */

#define SETG1(camel,h)                                          \
      case h:                                                   \
        {                                                       \
          SETH (h);                                             \
          XML_Set ## camel ## Handler (p, M (generic_ ## h));   \
        }                                                       \
      break

#define SETG2(camel,h,which)                                    \
      case h ## _ ## which:                                     \
        {                                                       \
          SETH (h ## _ ## which);                               \
          usep = (SPECIFIEDP (udsel (ud, h ## _start)) ||       \
                  SPECIFIEDP (udsel (ud, h ## _end)));          \
          XML_Set ## camel ## Handler                           \
            (p,                                                 \
             M (generic_ ## h ## _start),                       \
             M (generic_ ## h ## _end));                        \
        }                                                       \
      break

      switch (SCM_INUM (CDR (scm_assq (sym, halist))))
        {
          SETG2 (Element, element, start);
          SETG2 (Element, element, end);
          SETG1 (CharacterData, character_data);
          SETG1 (ProcessingInstruction, processing_instruction);
          SETG1 (Comment, comment);
          SETG2 (CdataSection, cdata_section, start);
          SETG2 (CdataSection, cdata_section, end);
          SETG1 (Default, lastchoice);
          SETG1 (NiceDefaultExpand, lastchoice_expand);
          SETG1 (UnparsedEntityDecl, unparsed_entity_decl);
          SETG1 (NotationDecl, notation_decl);
          SETG2 (NamespaceDecl, namespace_decl, start);
          SETG2 (NamespaceDecl, namespace_decl, end);
          SETG1 (NotStandalone, not_standalone);
          SETG1 (ExternalEntityRef, external_entity_ref);
          SETG1 (NiceUnknownEncoding, unknown_encoding);
        }

#undef SETH
#undef M
#undef SETG1
#undef SETG2
    }

#undef POPTO

  RETURN_UNSPECIFIED ();
#undef FUNC_NAME
}

PRIMPROC
(set_encoding, "set-encoding", 2, 0, 0,
 (SCM parser, SCM encoding),
 doc: /***********
Set encoding for @var{parser} to @var{encoding} (a string).  */)
{
#define FUNC_NAME s_set_encoding
  XML_Parser p;

  VALIDATE_PARSER ();
  SCM_VALIDATE_STRING (1, encoding);

  return SCM_MAKINUM (XML_SetEncoding (p, SCM_CHARS (encoding)));
#undef FUNC_NAME
}

PRIMPROC
(set_base, "set-base", 2, 0, 0,
 (SCM parser, SCM base),
 doc: /***********
Set base for @var{parser} to @var{base}.  */)
{
#define FUNC_NAME s_set_base
  XML_Parser p;

  VALIDATE_PARSER ();
  SCM_VALIDATE_STRING (1, base);

  return SCM_MAKINUM (XML_SetBase (p, SCM_CHARS (base)));
#undef FUNC_NAME
}

PRIMPROC
(get_base, "get-base", 1, 0, 0,
 (SCM parser),
 doc: /***********
Return the base (a string) of @var{parser}.
If none is set, return @code{#f}.  */)
{
#define FUNC_NAME s_get_base
  const char *base;
  XML_Parser p;

  VALIDATE_PARSER ();

  base = XML_GetBase (p);
  return STRMAYBE (base);
#undef FUNC_NAME
}

PRIMPROC
(get_specified_attribute_count, "get-specified-attribute-count", 1, 0, 0,
 (SCM parser),
 doc: /***********
Get the specified attribute count for @var{parser}.  */)
{
#define FUNC_NAME s_get_specified_attribute_count
  XML_Parser p;

  VALIDATE_PARSER ();

  return SCM_MAKINUM (XML_GetSpecifiedAttributeCount (p));
#undef FUNC_NAME
}

PRIMPROC
(parse, "parse", 2, 1, 0,
 (SCM parser, SCM s, SCM finalp),
 doc: /***********
Use @var{parser} to parse string @var{s}.
Optional third arg @var{finalp}, if non-@code{#f}, means
this call is the last parsing to be done on @var{s}.  */)
{
#define FUNC_NAME s_parse
  XML_Parser p;

  VALIDATE_PARSER ();

  SCM_VALIDATE_STRING (2, s);
  UNBOUND_MEANS_UNSPECIFIED (finalp);

  return SCM_MAKINUM (XML_Parse (p, ROZT (s), SCM_ROLENGTH (s),
                                 SPECIFIED_NOT_FALSE (finalp)));
#undef FUNC_NAME
}

PRIMPROC
(parse_buffer, "parse-buffer", 2, 1, 0,
 (SCM parser, SCM len, SCM finalp),
 doc: /***********
Use @var{parser} to parse @var{len} bytes of the internal buffer.
Optional third arg @var{finalp}, if non-@code{#f},
means this call is the last parsing to be done.  */)
{
#define FUNC_NAME s_parse_buffer
  XML_Parser p;

  VALIDATE_PARSER ();
  SCM_VALIDATE_INUM (2, len);
  UNBOUND_MEANS_UNSPECIFIED (finalp);

  return SCM_MAKINUM (XML_ParseBuffer (p, SCM_INUM (len),
                                       SPECIFIED_NOT_FALSE (finalp)));
#undef FUNC_NAME
}

static struct enumsym
param_entity_parsing_codes[] =
{
  PAIR (XML_PARAM_ENTITY_PARSING_NEVER),
  PAIR (XML_PARAM_ENTITY_PARSING_UNLESS_STANDALONE),
  PAIR (XML_PARAM_ENTITY_PARSING_ALWAYS)
};

static int
  param_entity_parsing_codes_count =
  sizeof (param_entity_parsing_codes) / sizeof (struct enumsym);

PRIMPROC
(set_param_entity_parsing, "set-param-entity-parsing", 2, 0, 0,
 (SCM parser, SCM code),
 doc: /***********
Set entity parsing for @var{parser} to @var{code} (a symbol).
This controls parsing of parameter entities (including the external
DTD subset).  See xmlparse.h for a complete description.  */)
{
#define FUNC_NAME s_set_param_entity_parsing
  XML_Parser p;
  int intcode = -1;
  int i;

  VALIDATE_PARSER ();

  for (i = 0; i < param_entity_parsing_codes_count; i++)
    {
      if (EQ (param_entity_parsing_codes[i].sy, code))
        {
          intcode = i;
          break;
        }
    }

  return SCM_MAKINUM (XML_SetParamEntityParsing (p, intcode));
#undef FUNC_NAME
}

static struct enumsym
error_codes[] =
{
  PAIR (XML_ERROR_NONE),
  PAIR (XML_ERROR_NO_MEMORY),
  PAIR (XML_ERROR_SYNTAX),
  PAIR (XML_ERROR_NO_ELEMENTS),
  PAIR (XML_ERROR_INVALID_TOKEN),
  PAIR (XML_ERROR_UNCLOSED_TOKEN),
  PAIR (XML_ERROR_PARTIAL_CHAR),
  PAIR (XML_ERROR_TAG_MISMATCH),
  PAIR (XML_ERROR_DUPLICATE_ATTRIBUTE),
  PAIR (XML_ERROR_JUNK_AFTER_DOC_ELEMENT),
  PAIR (XML_ERROR_PARAM_ENTITY_REF),
  PAIR (XML_ERROR_UNDEFINED_ENTITY),
  PAIR (XML_ERROR_RECURSIVE_ENTITY_REF),
  PAIR (XML_ERROR_ASYNC_ENTITY),
  PAIR (XML_ERROR_BAD_CHAR_REF),
  PAIR (XML_ERROR_BINARY_ENTITY_REF),
  PAIR (XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF),
  PAIR (XML_ERROR_MISPLACED_XML_PI),
  PAIR (XML_ERROR_UNKNOWN_ENCODING),
  PAIR (XML_ERROR_INCORRECT_ENCODING),
  PAIR (XML_ERROR_UNCLOSED_CDATA_SECTION),
  PAIR (XML_ERROR_EXTERNAL_ENTITY_HANDLING),
  PAIR (XML_ERROR_NOT_STANDALONE),
  PAIR (XML_ERROR_UNEXPECTED_STATE),
  PAIR (XML_ERROR_ENTITY_DECLARED_IN_PE),
  PAIR (XML_ERROR_FEATURE_REQUIRES_XML_DTD),
  PAIR (XML_ERROR_CANT_CHANGE_FEATURE_ONCE_PARSING),
  PAIR (XML_ERROR_UNBOUND_PREFIX),
  PAIR (XML_ERROR_UNDECLARING_PREFIX),
  PAIR (XML_ERROR_INCOMPLETE_PE),
  PAIR (XML_ERROR_XML_DECL),
  PAIR (XML_ERROR_TEXT_DECL),
  PAIR (XML_ERROR_PUBLICID),
  PAIR (XML_ERROR_SUSPENDED),
  PAIR (XML_ERROR_NOT_SUSPENDED),
  PAIR (XML_ERROR_ABORTED),
  PAIR (XML_ERROR_FINISHED),
  PAIR (XML_ERROR_SUSPEND_PE)
#if 2 == XML_MAJOR_VERSION
  ,
  PAIR (XML_ERROR_RESERVED_PREFIX_XML),
  PAIR (XML_ERROR_RESERVED_PREFIX_XMLNS),
  PAIR (XML_ERROR_RESERVED_NAMESPACE_URI)
#endif  /* 2 == XML_MAJOR_VERSION */
};

static int
  error_codes_count =
  sizeof (error_codes) / sizeof (struct enumsym);

/*
 * XML_Error
 */
static SCM
error_int_to_symbol (int code)
{
  return (error_codes_count > code
          ? error_codes[code].sy
          : SCM_BOOL_F);
}

static int
error_symbol_to_int (SCM sym)
{
  int code;

  for (code = 0; code < error_codes_count; code++)
    if (EQ (error_codes[code].sy, sym))
      return code;
  return -1;
}

PRIMPROC
(get_error_code, "get-error-code", 1, 0, 0,
 (SCM parser),
 doc: /***********
Return a symbol corresponding to the error code for @var{parser}.  */)
{
#define FUNC_NAME s_get_error_code
  XML_Parser p;

  VALIDATE_PARSER ();

  return error_int_to_symbol (XML_GetErrorCode (p));
#undef FUNC_NAME
}

PRIMPROC
(get_locus, "get-locus", 1, 1, 0,
 (SCM parser, SCM stash),
 doc: /***********
Return ``current'' locus information for @var{parser}
as a vector of four elements (all non-negative integers):

@example
#(LINE COLUMN BYTE-COUNT BYTE-INDEX)
@end example

Optional arg @var{stash} specifies a vector to fill in rather
than constructing a new one.  If an element in @var{stash} is
@code{#f}, the respective slot is skipped (it remains @code{#f}).  */)
{
#define FUNC_NAME s_get_locus
  XML_Parser p;
  SCM v;

  VALIDATE_PARSER ();
  UNBOUND_MEANS_UNSPECIFIED (stash);
  if (SPECIFIEDP (stash) && NOT_FALSEP (stash))
    {
      SCM_VALIDATE_VECTOR (2, stash);
      v = stash;
    }
  else
    v = scm_make_vector (SCM_MAKINUM (4), SCM_UNSPECIFIED);

#define JAM(n,f)                                                \
  if (NOT_FALSEP (VECREF (v, SCM_MAKINUM (n))))                 \
    scm_vector_set_x (v, SCM_MAKINUM (n), SCM_MAKINUM (f (p)))

  JAM (0, XML_GetCurrentLineNumber);
  JAM (1, XML_GetCurrentColumnNumber);
  JAM (2, XML_GetCurrentByteCount);
  JAM (3, XML_GetCurrentByteIndex);

#undef JAM

  return v;
#undef FUNC_NAME
}

PRIMPROC
(error_string, "error-string", 1, 0, 0,
 (SCM code),
 doc: /***********
Return a string representing the error @var{code} (a symbol).
If @var{code} is not recognized, return @code{#f}.  */)
{
#define FUNC_NAME s_error_string
  /* TODO: Handle multibyte XML_LChar.  */
  const XML_LChar *s;

  SCM_VALIDATE_SYMBOL (1, code);

  return (s = XML_ErrorString (error_symbol_to_int (code)))
    ? STRING (s)
    : SCM_BOOL_F;
#undef FUNC_NAME
}


/* Init.  */

static void
init_gexpat (void)
{
  int code;

  DEFSMOB (parser_tag, "XML-Parser",
           mark_parser, free_parser, print_parser);
  DEFSMOB (encoding_tag, "XML-Encoding",
           mark_encoding, free_encoding, print_encoding);

  halist = SCM_EOL;
  while (n_hnames--)
    halist = scm_acons (PERMANENT (SYMBOL (hnames[n_hnames])),
                        SCM_MAKINUM (n_hnames),
                        halist);
  halist = PERMANENT (halist);

  for (code = 0; code < param_entity_parsing_codes_count; code++)
    param_entity_parsing_codes[code].sy =
      PERMANENT (SYMBOL (param_entity_parsing_codes[code].nm));

  for (code = 0; code < error_codes_count; code++)
    error_codes[code].sy =
      PERMANENT (SYMBOL (error_codes[code].nm));

#include "expat.x"
}

#ifdef LAME_LAME_LAME
void
init_mixp_expat_module (void)
{
  init_gexpat ();
}
#else
MOD_INIT_LINK_THUNK ("mixp expat"
                     ,mixp_expat
                     ,init_gexpat);
#endif

/*
 * Local variables:
 * coding: utf-8
 * End:
 */

/* expat.c ends here */
