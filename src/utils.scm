;;; (mixp utils)

;; Copyright (C) 2007, 2009, 2010, 2011 Thien-Thi Nguyen
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

(define-module (mixp utils)
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
  #:use-module ((ice-9 streams) #:select (port->stream
                                          make-stream
                                          stream-car
                                          stream-cdr
                                          stream-null?
                                          stream-map
                                          vector->stream
                                          list->stream))
  #:use-module ((ice-9 rdelim) #:select (read-line))
  #:use-module ((mixp expat) #:prefix expat: #:select (parser?
                                                       parser-create
                                                       hset!
                                                       parse
                                                       error-symbol)))

(define (fs s . args)
  (apply simple-format #f s args))

(define (accumulator)
  (let* ((ls (list #f))
         (tp ls))
    (lambda stuff
      (cond ((null? stuff) (cdr ls))
            (else (set-cdr! tp stuff)
                  (set! tp (last-pair tp)))))))

;; Is it an "unsigned byte stream"?

(define ubs? (make-object-property))
(define (recognizable-as-ubs x)
  (set! (ubs? x) #t)
  x)

;; Return a stream delivering unsigned bytes from @var{source}.
;; @var{source} can be a port; a string; a list, vector or uniform
;; vector of unsigned bytes (integers in the range [0,255]), or a
;; pre-existing unsigned byte stream object.  For a list or vector,
;; the elements may also be Scheme characters, which are automatically
;; converted via @code{char->integer}.  This conversion also occurs
;; automatically if @var{source} is a string or a port.  For a uniform
;; vector (which must have prototype @code{#\nul}), elements are taken
;; modulo 256 i.e., @code{#y(-1 2 -42)} yields the three integers 255,
;; 2 and 214.
;;
;; Normally, @var{source} elements are delivered without validation.
;; If @var{check} (a symbol) is specified, however, if an element
;; is not an integer in the range [0,255], throw @var{check} with the
;; element as arg.
;;
;;-sig: (source [kwopts...])
;;
(define (make-unsigned-byte-stream source . check)

  (define (ubs<-char-stream s)
    (stream-map char->integer s))

  (define (from-port port)
    (ubs<-char-stream (port->stream port read-char)))

  (let ((s (cond ((port? source)
                  (from-port source))
                 ((string? source)
                  (call-with-input-string source from-port))
                 ((pair? source)
                  ((if (char? (car source))
                       ubs<-char-stream
                       identity)
                   (list->stream source)))
                 ((vector? source)
                  ((if (or (zero? (vector-length source))
                           (not (char? (vector-ref source 0))))
                       identity
                       ubs<-char-stream)
                   (vector->stream source)))
                 ((and (uniform-vector? source)
                       (eq? #\nul (array-prototype source)))
                  (let ((len (uniform-vector-length source)))
                    (make-stream
                     (lambda (i)
                       (or (= len i)
                           (cons (modulo (uniform-vector-ref source i) 256)
                                 (1+ i))))
                     0)))
                 ((ubs? source)
                  source)
                 (else
                  (error "bad source:" source)))))
    (recognizable-as-ubs
     (cond ((and (not (null? check))
                 (symbol? check)
                 (car check))
            => (lambda (key)
                 (stream-map (lambda (ub)
                               (or (and (integer? ub)
                                        (<= 0 ub 255))
                                   (throw key ub))
                               ub)
                             s)))
           (else s)))))

;; Return a procedure that internally parses the UTF8-encoded input
;; @var{from}, which may be a string, a port, a list of characters, a
;; list of unsigned bytes (integers in the range [0,255]), or the
;; ``extracted ubs'' from a previous call to @code{utf8-reader} (see
;; below).
;;
;; Optional arg INIT-POS is a pair @code{(@var{byte} . @var{char})},
;; defaulting to (0 . 0), that specifies the stream's initial position.
;;
;; If the returned procedure @var{p} is called with no arguments,
;; it reads the next UTF8 encoded character (one or more bytes),
;; updates the stream position, updates the reused-storage pair:
;; @example
;; (BYTES-CONSUMED . UVAL)
;; @end example
;; and returns it.  Both @var{byte-count} and @var{uval} are
;; integers.  If there is a problem, @var{p} throws @code{invalid-utf8}
;; with the list of problematic bytes.  Note that decoding does not
;; perform UCS-specific checks (see @code{valid-ucs?} et al for that).
;; On the other hand, if the stream is empty, @var{p} returns @code{#f}.
;;
;; Internally, for speed, storage for stream state is allocated once
;; and subsequently reused.  To access this state, @var{p} can also be
;; called with @var{command} @var{args}@dots{}, where @var{command} is
;; one of:
;;
;; @table @code
;; @item #:raw-bytes
;; This returns the vector (length six) used to temporarily store the
;; bytes taken from the stream.  Some elements may be unspecified,
;; and some elements may represent bytes from previous calls.  On
;; @code{invalid-utf8} error, the proper subset is also thrown.
;;
;; @item #:posbox
;; This returns the pair @code{(@var{byte} . @var{char})}, where
;; both @var{byte} and @var{char} are counters of bytes and characters
;; parsed, respectively.
;;
;; @item #:rvbox
;; This returns the pair @code{(@var{bytes-consumed} . @var{uval})},
;; the same one (updated and) returned by calling @var{p} with no
;; arguments.
;;
;; @item #:ubs
;; This returns the unsigned-byte stream object constructed from
;; @var{from}, marked in a way that it is recognizable if passed to
;; another call to @var{utf8-reader}.
;; @end table
;;
;; Aside from directly manipulating the state (which is not
;; recommended practice, by the way), two additional commands
;; provide common abstractions.
;;
;; @table @code
;; @item #:skip! @var{n} [@var{acc?}]
;; This advances the stream by @var{n} bytes or until the stream is
;; empty, whichever happens first.  The value is of the form:
;; @example
;; ((BEFORE . DIFF) [BYTE...])
;; @end example
;; where @var{before} is the byte-position before skipping, @var{diff}
;; is how many bytes were skipped (less than or equal to @var{n}), and
;; the rest the list of bytes skipped, if @var{acc?} is specified and
;; non-@code{#f}.
;;
;; @item #:sync!
;; This advances the stream (if necessary) until the ``current byte''
;; looks like the beginning of a valid UTF8-encoded character, or until
;; the stream is empty, whichever happens first, and returns a form
;; similar to that of the @code{#:skip!} command.
;; @end table
;;
;;-sig: (from [init-pos])
;;
(define (utf8-reader from . init-pos)
  (let ((posbox (if (null? init-pos)
                    (cons 0 0)
                    (cons (caar init-pos)
                          (cdar init-pos))))
        (rvbox (cons #f #f))
        (raw-bytes (make-vector 6))
        (ubs (make-unsigned-byte-stream from #:check 'invalid-utf8)))

    (define (advance!)
      (set-car! posbox (1+ (car posbox)))
      (set! ubs (stream-cdr ubs)))

    (define (ignored bef acc)
      (acons bef (- (car posbox) bef) (acc)))

    (define (skip! n . acc?)
      (let ((acc? (and (not (null? acc?)) (car acc?)))
            (acc (accumulator))
            (bef (car posbox)))
        (let loop ((n n))
          (cond ((or (zero? n) (stream-null? ubs))
                 (ignored bef acc))
                (else
                 (and acc? (acc (stream-car ubs)))
                 (advance!)
                 (loop (1- n)))))))

    (define (sync!)
      (let ((acc (accumulator))
            (bef (car posbox)))
        (let loop ()
          (cond ((or (stream-null? ubs)
                     (not (= 2 (bit-extract (stream-car ubs) 6 8))))
                 (ignored bef acc))
                (else
                 (acc (stream-car ubs))
                 (advance!)
                 (loop))))))

    (define (next-ubyte!)
      (and (not (stream-null? ubs))
           (let ((ub (stream-car ubs)))
             (advance!)
             ub)))

    (define (bad through)
      (apply throw 'invalid-utf8 (list-head (vector->list raw-bytes)
                                            through)))

    (define (got a d)
      (set-cdr! posbox (1+ (cdr posbox)))
      (set-car! rvbox a)
      (set-cdr! rvbox d)
      rvbox)

    (define (grok! one)
      (vector-set! raw-bytes 0 one)
      (and (or (memq one '(#xC0 #xC1))
               (<= #xF5 one #xFF))
           (bad 1))

      (if (> 128 one)

          ;; ASCII: Done!
          (got 1 one)

          ;; Process more bytes, taking ‘one’ as the control byte
          ;; (which has some data bits in the lower part as well).
          (let ((need #f)               ; how many valid in ‘more’
                (uval 0))

            (define (xlo n stop)
              (bit-extract n 0 stop))

            (define (xlo6 n)
              (xlo n 6))

            ;; Determine which additional bytes are to be read.
            ;; Valid results are in the range [2,6].
            (or (logbit? 6 one)
                (bad 1))
            (set! need (do ((i 5 (1- i)))
                           ((if (zero? i)
                                (bad 1)
                                (not (logbit? i one)))
                            (- 7 i))))

            ;; Start decoded value with data bits from control byte.
            (set! uval (xlo one (- 7 need)))

            ;; Read, validate and extract data bits from additional bytes.
            (do ((i 1 (1+ i)))
                ((= i need))
              (let ((b (next-ubyte!)))
                (vector-set! raw-bytes i b)
                (or (and b (= #b10 (bit-extract b 6 8)))
                    (bad i))
                (set! uval (logior (ash uval 6) (xlo6 b)))))

            ;; Done!
            (got need uval))))

    (lambda command
      (if (null? command)
          (and=> (next-ubyte!) grok!)
          (case (car command)
            ((#:raw-bytes) raw-bytes)
            ((#:posbox) posbox)
            ((#:rvbox) rvbox)
            ((#:ubs) (recognizable-as-ubs ubs))
            ((#:skip!) (apply skip! (cdr command)))
            ((#:sync!) (sync!))
            (else (error "bad command:" command)))))))

;; Return @code{#t} iff @var{uval} is a UCS value that is neither a
;; UTF-16 surrogate (in the range [#xD800,#xDFFF]) nor a non-character
;; (one of #xFFFE, #xFFFF).
;;
(define (valid-ucs? uval)
  (not (or
        ;; UTF-16 surrogates: [#xD800,#xDFFF]
        (<= #xD8 (ash uval -8) #xDF)
        ;; Non characters: #xFFFE, #xFFFF
        (memq uval '(#xFFFE #xFFFF)))))

;; Return @code{#t} iff @var{uval} is a UCS value that fits in two bytes.
;; This uses @code{valid-ucs?}.
;;
(define (valid-ucs2? uval)
  (and (valid-ucs? uval)
       (> 65536 uval)))

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
           (throw (expat:error-symbol parser))))
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

;;; (mixp utils) ends here
