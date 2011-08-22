;;; (mixp simit)

;; Copyright (C) 2011 Thien-Thi Nguyen
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

;;; Code:

(define-module (mixp simit)
  #:export (from-port)
  #:use-module ((ice-9 rdelim) #:select (read-line))
  #:use-module ((mixp expat) #:prefix E: #:select (parser-create-ns
                                                   parse
                                                   error-symbol
                                                   get-locus
                                                   hset!)))

(define THE-XML-NAMESPACE '(xml "http://www.w3.org/XML/1998/namespace"))

(define (ns-pair wasteful)              ; (NICK "URI") => ("URI" . "NICK:")
  (cons (cadr wasteful)
        (string-append
         (symbol->string (car wasteful))
         ":")))

;; Parse an XML document from @var{port} with some @var{namespaces},
;; a list of elements each of the form @code{(@var{nick} @var{uri})},
;; where @var{nick} is a symbol, and @var{uri} is a string.
;;
;; The XML namespace is built-in:
;; @example
;; (xml "http://www.w3.org/XML/1998/namespace")
;; @end example
;;
;; If parsing is successful, return an SXML-ish tree.
;; Otherwise, throw @code{parse-error} with two args, the symbolic
;; @var{reason} (suitable for passing to @code{error-string}) and
;; the location of the error as returned by @code{get-locus}.
;;
(define (from-port port namespaces)
  (let ((nicks (map ns-pair `(,@namespaces ,THE-XML-NAMESPACE)))
        (parser (E:parser-create-ns "UTF-8" #\))
        (top-stuff '())
        (level 0)
        (ns (list #f))
        (stack '(())))

    (define (top-stuff! type x)
      (cond ((assq type top-stuff)
             => (lambda (pair)
                  (set-cdr! pair (cons x (cdr pair)))))
            (else
             (set! top-stuff (acons type (list x)
                                    top-stuff)))))

    (define (push! x)
      (set-car! stack (cons x (car stack))))

    (define (pop!)
      (let ((rv (car stack)))
        (set! stack (cdr stack))
        rv))

    (define (processing-instruction target data)
      ;; CHECKME: Is this the right place?
      (push! `(*PI* ,target ,data)))

    (define (symbolic-name name cur-ns)
      (string->symbol
       (cond ((string-index name #\)
              => (lambda (mid)
                   (let ((bef (substring name 0 mid))
                         (aft (substring name (1+ mid))))
                     (cond ((assoc-ref nicks bef)
                            => (lambda (prefix)
                                 (string-append prefix aft)))
                           (else
                            (string-append bef ":" aft))))))
             (cur-ns
              (string-append cur-ns name))
             (else
              name))))

    (define (element-start name attributes)
      (set! level (1+ level))
      (set! stack (cons '() stack))
      (push! (symbolic-name name (car ns)))
      (or (null? attributes)
          (push! `(@ ,@(map (lambda (pair)
                              (list (symbolic-name (car pair) #f)
                                    (cdr pair)))
                            attributes)))))

    (define (element-end name)
      (push! (reverse! (pop!)))
      (set! level (1- level)))

    (define (namespace-decl-start prefix uri)
      (and (zero? level)
           (top-stuff! '*NAMESPACES* (list (string->symbol prefix)
                                           uri)))
      (set! ns (cons (string-append prefix ":") ns))
      (set! nicks (acons uri prefix nicks)))

    (define (namespace-decl-end prefix)
      (set! nicks (cdr nicks))
      (set! ns (cdr ns)))

    (define (character-data string)
      (let ((ls (string->list string)))
        (or (and-map char-whitespace? ls)
            (let ((before (caar stack)))
              (if (string? before)
                  (set-car! (car stack) (string-append before string))
                  (push! string))))))

    (define (comment string)
      ;; CHECKME: Is this the right place?
      (push! `(*COMMENT* ,string)))

    (define (parse-data)
      (define (check s last?)
        (and (eq? 'XML_STATUS_ERROR (E:parse parser s last?))
             (list 'parse-error
                   (E:error-symbol parser)
                   (E:get-locus parser))))
      (let loop ()
        (let ((line (read-line port 'concat)))
          (cond ((or (not line)
                     (eof-object? line))
                 ;; last chunk of data
                 (check "" #t))
                ((check line #f))
                (else (loop))))))

    ;; Do it!
    (E:hset!
     parser
     `((processing-instruction . ,processing-instruction)
       (element-start . ,element-start)
       (element-end . ,element-end)
       (namespace-decl-start . ,namespace-decl-start)
       (namespace-decl-end . ,namespace-decl-end)
       (character-data . ,character-data)
       ;; Questionable.
       (comment . ,comment)

       ;; TODO:
       ;; cdata-section-start
       ;; cdata-section-end
       ;; lastchoice
       ;; lastchoice-expand
       ;; unparsed-entity-decl
       ;; notation-decl
       ;; not-standalone
       ;; external-entity-ref
       ;; unknown-encoding
       ))
    (cond ((parse-data) => (lambda (problem)
                             (apply throw problem)))
          (else `(*TOP* ,(if (null? top-stuff)
                             '(*PI* xml "version='1.0'")
                             `(@ ,@top-stuff))
                        ,@(car stack))))))

;;; (mixp simit) ends here
