;;; -*- mode: scheme; -*-
;;; The Original Code is Mixp.
;;;
;;; The Initial Developer of the Original Code is Thierry Bézecourt.
;;; Portions created by Thierry Bézecourt are Copyright (C) 1999, 2000
;;; Thierry Bézecourt. All Rights Reserved.
;;; 
;;; Copyright (C) 2002, 2003 Dmitry Morozhnikov <dmiceman@mail.ru>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;; Boston, MA 02111-1307 USA

(define-module (xml expat)
 :export (mixp:parser? expat:parser-create expat:parser-create-ns
          expat:set-element-handler expat:set-character-data-handler
          expat:set-processing-instruction-handler
          expat:set-comment-handler expat:set-cdata-section-handler
          expat:set-default-handler expat:set-default-handler-expand
          expat:set-unparsed-entity-decl-handler
          expat:set-notation-decl-handler expat:set-notation-decl-handler
          expat:set-not-standalone-handler
          expat:set-external-entity-ref-handler
          expat:set-external-entity-ref-handler-arg
          expat:set-unknown-encoding-handler expat:default-current
          expat:set-user-data expat:get-user-data expat:set-encoding
          expat:use-parser-as-handler-arg
          expat:set-base get-base expat:get-specified-attribute-count
          expat:parse expat:parse-buffer expat:set-param-entity-parsing
          expat:get-error-code expat:get-current-byte-count
          expat:get-current-line-number expat:get-current-column-number
          expat:error-string mixp:expat-error-code make-xml-encoding))

(load-extension "libmixp" "scm_init_xml_expat")
