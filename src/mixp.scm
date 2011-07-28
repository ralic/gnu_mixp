;;; mixp.scm

;; Copyright (C) 2007, 2009, 2010 Thien-Thi Nguyen
;; Portions Copyright (C) 1999, 2000 Thierry Bézecourt
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this package; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA  02110-1301  USA.

;;; Author: Thierry Bézecourt
;;;     Thien-Thi Nguyen

;;; Code:

(define-module (ttn-do zz xml mixp)
  #:export (parse-data
            utf8->latin1
            utf8->ucs2
            utf8->ucs4
            xml->tree
            xml->list
            list->tree/one
            tree->list/one
            list->tree
            tree->list)
  #:use-module ((ice-9 rdelim) #:select (read-line))
  #:use-module ((ttn-do zzz emacsdream) #:select (utf8-reader
                                                  valid-ucs2?))
  #:use-module ((ttn-do zzz personally) #:select (accumulator
                                                  fs))
  #:use-module ((ttn-do zz xml expat) #:prefix expat:
                #:select (parser?
                          parser-create
                          hset!
                          parse
                          get-error-code)))

(define (get-parser opt proc)
  (cond ((null? opt)
         (expat:parser-create))
        ((and (= 1 (length opt)) (expat:parser? (car opt)))
         (car opt))
        (else
         (error (fs "Bad optional argument to ~A: ~A"
                    (procedure-name proc) opt)))))

;; Read all bytes from @var{port} (until it yields the EOF object), and
;; throw an error if the input does not represent a valid XML document.
;;
;;-sig: (port [parser])
;;
(define (parse-data port . args)
  (define (next)
    (read-line port 'concat))
  (let ((parser (get-parser args parse-data)))
    (define (check s last?)
      (and (zero? (expat:parse parser s last?))
           (throw (expat:get-error-code parser))))
    (let loop ((line (next)))
      (cond ((not line))
            ((eof-object? line))
            (else (check line #f)
                  (loop (next)))))
    ;; last chunk of data
    (check "" #t)))

;; Convert the byte list @var{from} from UTF-8 to Latin-1.  Throw
;; @code{invalid-utf8} if @var{from} is not a valid UTF-8 stream, and
;; @code{no-latin1} if one of the characters is a multi-byte character
;; (and thus cannot be a Latin-1 character).  If @var{from} is a string,
;; return a string.  If @var{from} is a list, return a list.
;;
(define (utf8->latin1 from)
  (let* ((next (utf8-reader from))
         (res (next #:rvbox))
         (acc (accumulator)))
    (let loop ()
      (and (next)
           (let ((uval (cdr res)))
             (or (> 256 uval)
                 (throw 'no-latin1 (car res) uval))
             (acc (integer->char uval))
             (loop))))
    ((if (string? from)
         list->string
         identity)
     (acc))))

;; Convert a UTF-8 string, such as those returned by the parser, to a
;; UCS-2 list.  @var{from} may be a string or a list.  Return a
;; list whose elements are sub-lists with length two, each encoding a
;; character from the original stream.  Throw a @code{no-ucs2} error if
;; one of the characters decoded from the UTF-8 string is not a UCS-2
;; character.
;;
(define (utf8->ucs2 from)
  (let* ((next (utf8-reader from))
         (res (next #:rvbox))
         (acc (accumulator)))
    (let loop ()
      (and (next)
           (let ((uval (cdr res)))
             (or (valid-ucs2? uval)
                 (throw 'no-ucs2 (car res) uval))
             (acc (list (integer->char (ash uval -8))
                        (integer->char (logand #xFF uval))))
             (loop))))
    (acc)))

;; Convert a UTF-8 string, such as those returned by the parser, to a
;; UCS-4 stream.  @var{from} may be a string or a list.  Return a
;; list whose elements are sub-lists with length four, each encoding a
;; character from the original stream.
;;
(define (utf8->ucs4 from)
  (let* ((next (utf8-reader from))
         (res (next #:rvbox))
         (acc (accumulator)))
    (let loop ()
      (and (next)
           (let ((uval (cdr res))
                 (sub '()))
             (do ((i 0 (1+ i)))
                 ((= 4 i))
               (set! sub (cons (integer->char (logand #xFF uval)) sub))
               (set! uval (ash uval -8)))
             (acc sub)
             (loop))))
    (acc)))

;;;
;;; Build a data structure from an XML document.
;;; Implementation note: flat lists and trees should be equivalent,
;;; i.e it should be possible to build a list from a tree and a tree
;;; from a list without losing information.
;;;

;; Build a tree data structure from the XML document read from
;; @var{port}.  Each XML element produces a new branch in the tree.
;;
;;-sig: (port [parser])
;;
(define (xml->tree port . args)
  (let ((xml-list (apply xml->list port args)))
    (list->tree/one xml-list)))

;; Transform a list of XML nodes @var{node-list} into a hierarchical
;; data structure.
;;
(define (list->tree/one node-list)
  (list->tree node-list '(())))

(define (list->tree node-list result)
  ;; Note: this function is tail-recursive (hence the "result"
  ;; argument), in order to handle XML document of any size.
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
            pi
            comment)
           (let ((level (if (null? result) '() (car result)))
		 (up-levels (if (null? result) '() (cdr result))))
	     (list->tree (cdr node-list)
			 (cons (append level
				       (list (car node-list)))
			       up-levels))))
	  (else (throw 'mixp:invalid-node-type))))))

;; Build a list of XML nodes from the @var{tree} of XML nodes, as
;; returned by @code{list->tree/one}.
;;
(define (tree->list/one tree)
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
              pi
              comment)
	     (tree->list (cdr tree)
			 (cons current-node result)
			 other-branch
			 cur-elems))
	    (else (throw 'mixp:invalid-node-type)))))))

;; Build a list from the XML document read from @var{port}.  The list
;; contains elements of the form @code{(@var{tag} @var{rest}...)}
;; with the following tags:
;;
;; @table @code
;; @item start-element
;; @itemx end-element
;; Start and end elements, respectively
;; @item character-data
;; Character data
;; @item notation-decl
;; Notation declaration
;; @item entity-decl
;; Unparsed-entity declaration
;; @item pi
;; Processing instruction
;; @item comment
;; Comment
;; @end table
;;
;;-sig: (port [parser])
;;
(define (xml->list port . args)
  (let ((parser (get-parser args xml->list))
	(nodes (accumulator)))

    (define (tagged tag)
      (lambda rest
        (nodes (cons tag rest))))

    (expat:hset!
     parser
     'element-start          (tagged 'start-element)
     'element-end            (tagged 'end-element)
     'character-data         (tagged 'character-data)
     'notation-decl          (tagged 'notation-decl)
     'unparsed-entity-decl   (tagged 'entity-decl)
     'processing-instruction (tagged 'pi)
     'comment                (tagged 'comment))
    ;; TBD: other handlers...
    (parse-data port parser)
    (nodes)))

;;; Local variables:
;;; coding: utf-8
;;; End:

;;; mixp.scm ends here
