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

(define-module (xml mixp)
  :use-module (ice-9 format)
  :use-syntax (ice-9 syncase) ;; :use-module does not work here !
  :use-module (xml expat)
  :use-module (ice-9 rdelim))

(export mixp:parse
	mixp:parse-data
	mixp:parse-file
	mixp:call-with-input-string
	mixp:call-with-input-file
	mixp:utf8->latin1
	mixp:utf8->ucs2
	mixp:utf8->ucs4
	
	mixp:xml->tree
	mixp:xml->list
	mixp:list->tree
	mixp:tree->list

	list->tree
	tree->list

    ;; Bug fixed by Christian Neukirchen <chneukirchen@yahoo.de>
    ;; on 11jan2003. Due a bug in Guile 1.6.1, all macros have to
	;; be exported.
	mixp:dispf
	mixp:bit-eqv?
)

(define trace #t)

(define-syntax mixp:dispf
  (syntax-rules ()
    ((mixp:dispf fmt ...) (if trace
			 (begin (display (format fmt ...))
				(newline))))))

(define (set-trace flag)
  (set! trace flag))

;; Maybe this should be in the Guile core / maybe it is and I couldn't
;; see it...
(define (prepend! l elems)
  "Prepends ELEMS to the list L, and returns L.  If L is not the empty list,
then this procedure is destructive, i.e L is modified in place."
  (cond ((null? l) (list elems))
	((null? elems) l)
	(else (set-cdr! l (append (cdr elems)
				  (cons (car l) (cdr l))))
	      (set-car! l (car elems))
	      l)))

; Checks that ARGS either contains zero element, and returns a new
; parser object, or contains one parser element, and returns it.
(define (args->parser args proc-name)
  (if (or (> (length args) 1)
	  (and (= (length args) 1)
	       (not (mixp:parser? (car args)))))
      (error (format "Bad optional argument to ~A: ~A"
		     proc-name
		     args)))
  (if (null? args)
      (expat:parser-create)
      (car args)))

(define (mixp:call-with-input-string string parser)
  "Parse a string containing an XML document.

PARSER should be created with another procedure such as
expat:parser-create."
  (if (not (mixp:parser? parser))
      (error (format "~A: not a parser in mixp:call-with-input-string call")))
  (call-with-input-string string (lambda (port)
				   (mixp:parse-data port parser))))

(define (mixp:call-with-input-file file parser)
  "Parse an XML file.

PARSER should be created with another procedure such as expat:parser-create."
  (if (not (mixp:parser? parser))
      (error (format "~A: not a parser in mixp:call-with-input-string call")))
  (call-with-input-file file (lambda (port)
			       (mixp:parse-data port parser))))

(define (mixp:parse parser port)
  "Parses data coming from a port, and throws an error if the
document is not valid. source may be an XML document or a input port.

This procedure is deprecated.  Please use mixp:parse-data"
  (mixp:parse-data port parser))

(define (mixp:parse-data port . args)
  "Parses data coming from a port, and throws an error if the
document is not valid. source may be an XML document or a input port.

PORT is a port.  If ARGS is specified, it should contain one parser
element, which may have been created with expat:parser-create. No
handler should have been specified on this parser."
  (let ((parser (args->parser args "mixp:parse-data"))
	(line ""))
    (while (and (set! line (read-line port 'concat))
		(not (eof-object? line)))
	   (let ((res-parse (expat:parse parser line #f)))
	     (if (eqv? res-parse 0)
		 (throw (expat:get-error-code parser)))))
    ;; last chunk of data
    (if (eqv? (expat:parse parser "" #t) 0)
	(throw (expat:get-error-code parser)))))

(define (mixp:parse-file parser file)
  "Parses a XML file, and throws an error if the document is not valid"
  (mixp:call-with-input-file file parser))

(define (mixp:utf8->latin1 from)
  "Converts a byte list from UTF-8 to Latin-1. Throws 'invalid-utf8 if
FROM is not a valid UTF-8 stream, and 'no-latin1 if one of the
characters is a multi-byte character (and thus cannot be a Latin-1
character). If FROM is a string, the results is a string. If FROM is a
list, the results is a list."
  (if (string? from)
      (list->string (mixp:utf8->latin1
		     (string->list from)))
      (if (null? from)
	  '()
	  (let ((res (read-utf8 from)))
	    (if (or (equal? (length (cdr res)) 1)
		    (and (equal? (length (cdr res)) 2)
			 (equal? (cadr res) #\nul)))
		(cons (car (last-pair res)) (mixp:utf8->latin1
				 (list-tail from (car res))))
		(throw 'no-latin1))))))

(define (make-ucs-char in n)
  "Converts the 1-, 2-, 3- or 4-byte list IN into a 4-byte list,
by prepending zeroes as needed. IN is not copied, and it should not contain more than 4 bytes"
  (let ((len (length in)))
    (if (>= len n)
	in
	(let ((res (make-list (- n len) #\nul)))
	  (set-cdr! (last-pair res) in)
	  res))))
 
(define (mixp:utf8->ucs2 from)
  "Converts an UTF-8 string, such as those returned by the parser, to
a UCS-2 list. FROM may be a string or a list. The result is a list
which cells are lists with two elements. Each cell encodes one
character from the original stream. Throws a 'no-ucs2 error if one of
the character decoded from the UTF-8 string is not a UCS-2 character."
  (if (string? from)
      (mixp:utf8->ucs2 (string->list from))
      (if (null? from)
	  '()
	  (let ((res (read-utf8 from)))
	    (if (<= (length (cdr res)) 2)
		(cons (make-ucs-char (cdr res) 2)
		      (mixp:utf8->ucs2 (list-tail from (car res))))
		(throw 'no-ucs2))))))

(define (mixp:utf8->ucs4 from)
  "Converts an UTF-8 string, such as those returned by the parser, to
a UCS-4 stream. FROM may be a string or a list. The result is a list
which cells are lists with four elements. Each cell encodes one
character from the original stream."
  (if (string? from)
      (mixp:utf8->ucs4 (string->list from))
      (if (null? from)
	  '()
	  (let ((res (read-utf8 from)))
	    (cons (make-ucs-char (cdr res) 4)
		  (mixp:utf8->ucs4 (list-tail from (car res))))))))

(define (make-char first a b second c d)
  (logior (* (bit-extract first a b) (integer-expt 2 (- 8 (- b a))))
	  (bit-extract second c d)))

;; UTF-8 representation of a character, depending on the number of bytes
;;  in the character
;; FIXME: this procedure should detect sequences which enter in this pattern
;;  but are invalid (e.g 0xC080).
;; 0xxx xxxx
;; 110x xxyy 10yy yyyy
;; 1110 xxxx 10xx xxyy 10yy yyyy
;; 1111 0xxx 10xx yyyy 10yy yyzz 10zz zzzz
;; 1111 10xx 10yy yyyy 10yy zzzz 10zz zztt 10tt tttt
;; 1111 110x 10xx xxxx 10yy yyyy 10y zzzz 10zz zztt 10tt tttt

(define-syntax mixp:bit-eqv?
  (syntax-rules ()
    ((mixp:bit-eqv? from i j res) (eqv? (bit-extract from i j) res))))

(define (read-utf8 from-string)
  "Reads a UTF-8 sequence and returns the multi-byte character as a
list. The list may contain one to four bytes"
  (let* ((from (map char->integer from-string))
	 (len (length from)))
    (cond ((mixp:bit-eqv? (car from) 7 8 #b0)
	   (cons 1 (map integer->char
			(list (car from)))))
	  
	  ((and (mixp:bit-eqv? (car from) 5 8 #b110)
		(>= len 2)
		(mixp:bit-eqv? (cadr from) 6 8 #b10))
	   (cons 2 (map integer->char
			(list (bit-extract (car from) 2 5)
			      (make-char (car from) 0 2
					 (cadr from) 0 6)))))
	  
	  ((and (mixp:bit-eqv? (car from) 4 8 #b1110)
		(>= len 3)
		(mixp:bit-eqv? (cadr from) 6 8 #b10)
		(mixp:bit-eqv? (caddr from) 6 8 #b10))
	   (cons 3 (map integer->char
			(list (make-char (car from) 0 4
					 (cadr from) 2 6)
			      (make-char (cadr from) 0 2
					 (caddr from) 0 6)))))
	  
	  ((and (mixp:bit-eqv? (car from) 3 8 #b11110)
		(>= len 4)
		(mixp:bit-eqv? (cadr from) 6 8 #b10)
		(mixp:bit-eqv? (caddr from) 6 8 #b10)
		(mixp:bit-eqv? (cadddr from) 6 8 #b10))
	   (cons 4 (map integer->char
			(list (make-char (car from) 0 3
					 (cadr from) 4 6)
			      (make-char (cadr from) 0 4
					 (caddr from) 2 6)
			      (make-char (caddr from) 0 2
					 (cadddr from) 0 6)))))
	  
	  ((and (mixp:bit-eqv? (car from) 2 8 #b111110)
		(>= len 5)
		(mixp:bit-eqv? (list-ref from 1) 6 8 #b10)
		(mixp:bit-eqv? (list-ref from 2) 6 8 #b10)
		(mixp:bit-eqv? (list-ref from 3) 6 8 #b10)
		(mixp:bit-eqv? (list-ref from 4) 6 8 #b10))
	   (cons 5 (map integer->char
			(list (make-char (list-ref from 1) 0 6
					 (list-ref from 2) 4 6)
			      (make-char (list-ref from 2) 0 4
					 (list-ref from 3) 2 6)
			      (make-char (list-ref from 3) 0 2
					 (list-ref from 4) 0 6)))))
	  
	  ((and (mixp:bit-eqv? (car from) 1 8 #b1111110)
		(>= len 6)
		(mixp:bit-eqv? (list-ref from 1) 6 8 #b10)
		(mixp:bit-eqv? (list-ref from 2) 6 8 #b10)
		(mixp:bit-eqv? (list-ref from 3) 6 8 #b10)
		(mixp:bit-eqv? (list-ref from 4) 6 8 #b10)
		(mixp:bit-eqv? (list-ref from 5) 6 8 #b10))
	   (cons 6 (map integer->char
			(list (make-char (list-ref from 2) 0 6
					 (list-ref from 3) 4 6)
			      (make-char (list-ref from 3) 0 4
					 (list-ref from 4) 2 6)
			      (make-char (list-ref from 4) 0 2
					 (list-ref from 5) 0 6)))))
	  
	  (else (throw 'invalid-utf8)))))

;;;
;;; Build a data structure from an XML document.
;;; Implementation note: flat lists and trees should be equivalent,
;;; i.e it should be possible to build a list from a tree and a tree
;;; from a list without losing information.
;;;

(define (mixp:xml->tree port . args)
  "Builds a tree data structure from an XML document.  Each XML
element produces a new branch in the tree.

See mixp:xml->list for the XML nodes supported and the ARGS parameter."
  (let ((xml-list (apply mixp:xml->list port args)))
    (mixp:list->tree xml-list)))

;; Transforms a list of XML nodes into a hierarchical data structure
;; Implementation note: this function is tail-recursive (hence the
;; "result" argument), in order to handle XML document of any size.
(define (mixp:list->tree node-list)
  (list->tree node-list '(())))

(define (list->tree node-list result)
  (if (null? node-list)
      (car result)
      (let ((type (caar node-list)))
	(case type
	  ((start-element)
	   (list->tree (cdr node-list)
		       (cons (list 'element (cdar node-list))
			     result)))
	  ((end-element)
	   (let ((level (car result))
		 (up-levels (cdr result)))
	     (list->tree (cdr node-list)
			 (if (null? up-levels)
			     (list level)
			     (cons (append (car up-levels)
					   (list level)) 
				   (cdr up-levels))))))
	  ((character-data
	    notation-decl
	    entity-decl
    ;; TBD: The following definitions introduce memory management errors
;;	      pi
;;	      comment
	    )
	    (let ((level (if (null? result) '() (car result)))
		 (up-levels (if (null? result) '() (cdr result))))
	     (list->tree (cdr node-list)
			 (cons (append level
				       (list (car node-list)))  
			       up-levels))))
	  (else (mixp:dispf "Unexpected node type: ~A" type)
		throw 'mixp:invalid-node-type)))))

(define (mixp:tree->list tree)
  "Builds a list of XML nodes, as returned by mixp:xml->list, from a
tree of XML nodes, as returned by mixp:xml->tree."
  (tree->list tree))

(define (tree->list tree . args)
  (let ((result '())
	(other-branch '())
	(cur-elems '()))
    (if (not (null? args))
	(begin (set! result (car args))
	       (if (not (null? (cdr args)))
		   (begin (set! other-branch (cadr args))
			  (set! cur-elems (caddr args))))))
    (if (null? tree)
	(if (null? other-branch)
	    (reverse! result)
	    (tree->list (car other-branch)
			result
			(cdr other-branch)
			(cdr cur-elems)))
	(let* ((current-node (car tree))
	       (type (car current-node)))
	  (case type
	    ((element)
	     (tree->list (cddr current-node)
			 (cons (cons 'start-element
				     (cadr current-node)) 
			       result)
			 (cons (append `((end-element
					  ,(caadr current-node)))  
				       (cdr tree))
			       other-branch)
			 (cons (caadr current-node) cur-elems)))
	    ((end-element)
	     (tree->list (cdr tree)
			 (cons current-node result)
			 other-branch
			 cur-elems))
	    ((character-data
	      notation-decl
	      entity-decl
    ;; TBD: The following definitions introduce memory management errors
;;	      pi
;;	      comment
	      )
	     (tree->list (cdr tree)
			 (cons current-node result)
			 other-branch
			 cur-elems))
	    (else (mixp:dispf "Unexpected node type: ~A" type)
		  throw 'mixp:invalid-node-type)
	    )))))

(define (mixp:xml->list port . args)
  "Builds a list from an XML document.  The list contains one element
for each of the following XML element: start element, end element,
character data.  Other kinds of XML nodes are not (yet) implemented.

PORT is a port.  If ARGS is specified, it should contain one parser
element, which may have been created with expat:parser-create. No
handler should have been specified on this parser."
  (let ((parser (args->parser args "mixp:xml->list"))
	(node-list '(not-used)))
    (expat:set-element-handler parser
			       (lambda (data name attributes)
				 (prepend! data
					   `((start-element
					      ,name
					      ,attributes))))
			       (lambda (data name)
				 (prepend! data
					   `((end-element
					      ,name)))))
    (expat:set-character-data-handler parser
				      (lambda (data value)
					(prepend! data
						  `((character-data
						     ,value)))))
    (expat:set-notation-decl-handler parser
				     (lambda (data
					      notation-name
					      base
					      system-id
					      public-id) 
				       (prepend! data
						 `((notation-decl
						    ,notation-name
						    ,base
						    ,system-id
						    ,public-id)))))
    (expat:set-unparsed-entity-decl-handler parser
					    (lambda (data
						     entity-name
						     base
						     system-id
						     public-id
						     notation-name)
					      (prepend! data
							`((entity-decl
							   ,entity-name
							   ,base
							   ,system-id
							   ,public-id
							   ,notation-name)))))
    ;; TBD: The following definitions introduce memory management errors
;;    (expat:set-processing-instruction-handler parser
;;					      (lambda (data
;;						       target
;;						       pi-data)
;;						(prepend! data
;;							  `((pi
;;							     ,target
;;							     ,pi-data)))))
;;    (expat:set-comment-handler parser
;;			       (lambda (data comment-data)
;;				 (prepend! data
;;					   `((comment ,comment-data)))))
    ;; TBD: other handlers...
    (expat:set-user-data parser node-list)
    (mixp:parse parser port)
    (cdr (reverse! node-list))))
