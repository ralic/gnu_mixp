;;; -*- mode: scheme; -*-

(use-modules (xml expat)
             (xml mixp)
             (ice-9 format))
(use-syntax (ice-9 syncase))

;; A sample XML document used in several tests
(define my-document
  "<?xml version='1.0' encoding='ISO-8859-1' standalone='yes'?>
<!DOCTYPE doc [<!ENTITY program.name 'gexpat'>
               <!NOTATION SomeNotation PUBLIC 'nota'>]>
<!-- Comment for the sample document -->
<doc> <title> A sample XML document for &program.name; </title>

<chapter name='Chapter 1' id='1'>Contents of Chapter 1.<br/> </chapter>

<![CDATA[ba l> dsq>]]>

<chapter name='Chapter 2' id='2'>Contents of Chapter 2.</chapter>

</doc>")

;;; Miscellaneous
(define-syntax dispfmt
  (syntax-rules ()
    ((dispfmt fmt ...) (if trace
                           (begin (format #t fmt ...)
                                  (newline))))))

(define-syntax err
  (syntax-rules ()
    ((err fmt ...) (begin (format (current-error-port) fmt ...)
                          (newline)))))

(define trace-level 0)
(define (trace . args)
  (if (> trace-level 0)
      (map display args)))

(define test-counter 0)
(define test-failed '())
(define-syntax check
  (syntax-rules ()
    ((check b desc failed-msg)
     (begin
       (set! test-counter (1+ test-counter))
       (let ((res (catch #t
                         (lambda () b)
                         (lambda args (car args)))))
         (cond ((eq? res #t)
                (trace (format #t "ok ~S: ~A\n" test-counter desc)))
               ((eq? res #f)
                (begin (set! test-failed (cons (list test-counter
                                                     desc)
                                               test-failed))
                       (err "not ok ~S: ~A - ~A.  The expression evaluated was: ~S"
                            test-counter desc failed-msg 'b)))
               (#t
                (begin (set! test-failed (cons (list test-counter
                                                     desc)
                                               test-failed))
                       (err
                        "not ok ~S: ~A - ~A. A `~S' exception was received while evaluating: ~S\n"
                        test-counter desc failed-msg res 'b)))))))))

(define-syntax check-exc
  (syntax-rules ()
    ((check-exc b exc desc failed-msg)
     (begin
       (set! test-counter (+ test-counter 1))
       (let ((res (catch #t
                         (lambda () b)
                         (lambda args (car args)))))
         (cond ((eq? res #t)
                (begin (set! test-failed (cons (list test-counter
                                                     desc)
                                               test-failed))
                       (err "not ok ~S: ~A - ~A.  Missing exception `~S' while evaluating ~S\n"
                            test-counter desc failed-msg exc 'b)))
               ((eq? res #f)
                (begin (set! test-failed (cons (list test-counter
                                                     desc)
                                               test-failed))
                       (err "not ok ~S: ~A - ~A: missing exception `~S' while evaluating ~S\n"
                            test-counter desc failed-msg exc 'b)))
               ((eq? res exc)
                (trace (format #t "ok ~S: ~A - ~A\n"
                               test-counter desc failed-msg)))

               (#t
                (begin (set! test-failed (cons (list test-counter
                                                     desc)
                                               test-failed))
                       (dispfmt "not ok ~S: ~A - ~A. received unexpected answer `~S' while evaluating ~S\n"
                                test-counter desc failed-msg res 'b)))))))))

(define indent "")
(define (indent-increase)
  (set! indent (string-append indent "    ")))
(define (indent-decrease)
  (set! indent (substring indent 0 (- (string-length indent) 4))))

;;; Handlers
(define (my-start-element-handler data name atts)
  (trace indent "§ ELEMENT START '" name "' - attributes " atts
         " §\nData: " data "\n")
  (indent-increase))

(define (my-end-element-handler data name)
  (indent-decrease)
  (trace append indent "§ ELEMENT END '" name "' §\nData: " data "\n"))

(define (my-notation-decl-handler user-data notation-name base system-id public-id)
  '()
  (trace "§ NOTATION: user-data=" user-data
         ", notation-name="notation-name
         ", base=" base
         ", system-id" system-id
         ", public-id" public-id " §\n")
  (check (equal? user-data base)
         "user data and base"
         "Error, we did not receive the expected base"))

(define (my-start-element-handler-test-arg data name atts)
  (trace indent "§ ELEMENT START '" name "' - attributes " atts
         " §\nData: " data "\n")
  (if (not (mixp:parser? data)) (err "Error") (trace "Ok"))
  (indent-increase))

(define (my-end-element-handler-test-arg data name)
  (indent-decrease)
  (trace indent "§ ELEMENT END '" name "' §\nData: " data "\n"))

(define (my-character-data-handler data value)
  (trace indent "§ CHARACTER DATA '" value " §\n"))

(define (my-comment-handler data value)
  (trace indent "§ COMMENT '" value "' §\n"))

(define (my-start-cdata-section-handler data )
  (trace indent "§ CDATA START §\n")
  (indent-increase))

(define (my-end-cdata-section-handler data )
  (indent-decrease)
  (trace indent "§ CDATA END §\n"))

;; Still TBD.  Unknown encoding handlers are not easy to use
(define (my-unknown-encoding-handler encodingHandlerData name)
  (if (string=? name "thb-encoding")
      (let ((map (make-vector 256 67))
            (data '())
            (convert (lambda (data s) (trace "convert\n") 68))
            (release (lambda (data) (trace "release\n"))))
        (begin
          (trace "Recognized encoding " name "\n")
          (make-xml-encoding map data
                             convert release)))))

(define (test-handlers)
  ;; create the parser, and set the handlers
  (let ((my-parser (expat:parser-create)))
    (expat:set-element-handler my-parser
                               my-start-element-handler
                               my-end-element-handler)
    (expat:set-character-data-handler my-parser
                                      my-character-data-handler)
    (expat:set-comment-handler my-parser
                               my-comment-handler)
    (expat:set-cdata-section-handler my-parser
                                     my-start-cdata-section-handler
                                     my-end-cdata-section-handler)

;;; parse the document, and print the errors, if any

    (let ((res-parse (expat:parse my-parser my-document #t)))
      (check (not (= res-parse 0))
             "parse"
             (format #t "Parser error: ~A(~A)"
                     (expat:get-error-code my-parser)
                     (expat:error-string
                      (expat:get-error-code my-parser)))))))

(define (test-not-well-formed)
  (let ((bad-xml "<doc>dfssfd</do>"))
    (trace "Parsing a not well-formed document with expat:parse...\n")
    (let ((p (expat:parser-create)))
      (expat:set-character-data-handler p (lambda (u c)
                                            '()))
      (let ((res (expat:parse p bad-xml #t)))
        (check (equal? res 0)
               "Parse bad XML"
               "Parser did not return an error on not well-formed document")
        (if (equal? res 0)
            (let ((code (expat:get-error-code p)))
              (check (eqv? code 'expat:XML_ERROR_TAG_MISMATCH)
                     "Expect an 'expat:XML_ERROR_TAG_MISMATCH error"
                     (format #t "Expecting error code 'expat:XML_ERROR_TAG_MISMATCH, received ~A"
                             code))))))
    (trace "Parsing a not well-formed document with mixp:parse-data...\n")
    (check-exc (call-with-input-string bad-xml mixp:parse-data)
               'expat:XML_ERROR_TAG_MISMATCH
               "Expect an 'expat:XML_ERROR_TAG_MISMATCH exception"
               "Parser did not return the expected exception on not well-formed document")))

(define (test-unparsed-entities)
  (let ((xml-doc "<!DOCTYPE doc [
<!NOTATION vrml PUBLIC 'VRML 2'>
<!ENTITY Antarctica SYSTEM 'http://www.antarctica.net' NDATA vrml>
<!ATTLIST World src ENTITY #REQUIRED>]>
<doc>
 <World src='Antarctica' />
</doc>")
        (my-parser (expat:parser-create))
        (user-data '(not-used))
        (notation-decl-handler (lambda (user-data notation-name base
                                                  system-id public-id)
                                 (append! user-data
                                          `((,notation-name
                                             ,public-id)))))
        (unp-ent-decl-handler (lambda (user-data entity-name base
                                                 system-id public-id
                                                 notation-name)
                                (append! user-data
                                         `((,entity-name
                                            ,system-id))))))
    (expat:set-user-data my-parser user-data)
    (expat:set-notation-decl-handler my-parser notation-decl-handler)
    (expat:set-unparsed-entity-decl-handler my-parser unp-ent-decl-handler)
    (let ((res-parse (expat:parse my-parser xml-doc #t)))
      (check (not (= res-parse 0))
             "unparsed entities"
             (format #t "Parser error: ~A"
                     (expat:error-string
                      (expat:get-error-code my-parser))))
      (check (equal? (cdr (expat:get-user-data my-parser))
                     '(("vrml" "VRML 2")
                       ("Antarctica" "http://www.antarctica.net")))
             "unparsed entities"
             (format #t "Handler error: received the following user data: ~A"
                     (cdr (expat:get-user-data my-parser)))))))

(define (test-error-string)
  (let* ((code 'expat:XML_ERROR_UNCLOSED_TOKEN)
         (code-expected (mixp:expat-error-code 5))
         (msg (expat:error-string code)))
    (check (equal? code code-expected)
           "error string"
           (format #t "Returned code was ~A, expecting ~A"
                   code code-expected))
    (check (> (string-length msg) 1)
           "error string not empty"
           (format #t "Returned message was `~A'" msg))))

;;; creates and prints a xml-encoding structure, without parsing a
;;; document with it
(define (test-encoding)
  (let* ((map (make-vector 256 67))
         (data '())
         (convert (lambda (data s) (trace "convert\n") 68))
         (release (lambda (data) (trace "release\n")))
         (encoding (make-xml-encoding map data convert release)))
    (trace encoding)
    (trace "\n")
    #t))

(define (test-unknown-encoding)
  ;; 1. Wrong encoding in the document should raise an error
  (let* ((doc "<?xml version='1.0' encoding='US-ASCII'
standalone='yes'?> <body>Les Français ne sont pas ascii</body>")
         (my-parser (expat:parser-create))
         (res-parse (expat:parse my-parser doc #t)))
    (check (= res-parse 0)
           "Expect an encoding error"
           "Error, expecting an encoding error")
    (if (= res-parse 0)
        (let ((error-code (expat:get-error-code my-parser)))
          (check (eqv? error-code 'expat:XML_ERROR_INVALID_TOKEN)
                 "Ok, got an INVALID TOKEN error as expected\n"
                 (format #t "Error: expecting error code 4, got ~A"
                         error-code)))))
  ;; 2. Wrong encoding in the document overriden by a good encoding
  ;; in set-encoding should not raise an errore
  (let* ((doc "<?xml version='1.0' encoding='foo'
standalone='yes'?> <body>España no es un ascii país</body>")
         (my-parser (expat:parser-create)))
    (expat:set-encoding my-parser "ISO-8859-1")
    (let ((res-parse (expat:parse my-parser doc #t)))
      (check (not (= res-parse 0))
             "Parse with ISO-8859-1 encoding"
             (format #t "Parsing error ~A"
                     (expat:get-error-code my-parser)))))
  ;; 2. set-encoding with an inexistant encoding should raise a
  ;; specific error
  (let* ((doc "<?xml version='1.0' encoding='ISO-8859-1' standalone='yes'?> <body>Whatever</body>")
         (my-parser (expat:parser-create)))
    (expat:set-encoding my-parser "foo")
    (let ((res-parse (expat:parse my-parser doc #t)))
      (check (= res-parse 0)
             "Expect and encoding error"
             "Error, expecting an encoding error")
      (if (= res-parse 0)
          (let ((error-code  (expat:get-error-code my-parser)))
            (check (eqv? error-code 'expat:XML_ERROR_UNKNOWN_ENCODING)
                   "Expect an UNKNOWN ENCODING error"
                   (format #t "Error: expecting error code 18, got ~A"
                           error-code)))))))

;;; Sets a Scheme object as the user data, and retrieves it.
(define (test-user-data)
  (let ((my-parser (expat:parser-create))
        (user-data (lambda ()
                     (trace "Any Scheme object can be user data\n")
                     42)))
    (expat:set-user-data my-parser user-data)
    (let* ((new-user-data (expat:get-user-data my-parser))
           (n (new-user-data)))
      (check (equal? n 42)
             "User data"
             (format #t "Bad return value: ~A" n))
      (if (equal? n 42)
          (check (eqv? new-user-data user-data)
                 "User data\n"
                 (format #t "The returned user data is not exactly the same: ~A != ~A"
                         n user-data))))))

(define (test-use-parser-arg)
  (let ((my-parser (expat:parser-create))
        (user-data 42))
    (expat:set-user-data my-parser user-data)
    (expat:set-element-handler my-parser
                               my-start-element-handler-test-arg
                               my-end-element-handler-test-arg)
    (expat:use-parser-as-handler-arg my-parser)
    (let ((res-parse (expat:parse my-parser my-document #t)))
      (check (not (= res-parse 0))
             "Use parser"
             (format #t "Parser error: ~A"
                     (expat:error-string
                      (expat:get-error-code my-parser)))))))

;;; Unknown encoding handler: TBD
(define (test-encoding-parse)
  (let ((my-parser (expat:parser-create "thb-encoding"))
        (data '()))
    (expat:set-unknown-encoding-handler my-parser
                                        my-unknown-encoding-handler
                                        data)
    (let ((res-parse (expat:parse my-parser my-document #t)))
      (check (not (= res-parse 0))
             "Unknown encoding handler"
             (format #t "Parser error: ~A"
                     (expat:error-string
                      (expat:get-error-code my-parser)))))))

(define (test-base)
  (let ((my-parser (expat:parser-create))
        (my-base "My base"))
    (expat:set-notation-decl-handler my-parser
                                     my-notation-decl-handler)
    (expat:set-user-data my-parser my-base)
    (expat:set-base my-parser my-base)
    (let ((res-parse (expat:parse my-parser my-document #t)))
      (check (not (= res-parse 0))
             "Base"
             (format #t "Parser error: ~A"
                     (expat:error-string (expat:get-error-code my-parser)))))))

;; This test does not work, it's just an idea about a more convenient
;; API thant the current expat API
(define (test-xml-parse)
  (catch #t
         (lambda ()
           (mixp:parse my-document
                       `(handlers .
                                  ((element . (,my-start-element-handler
                                               ,my-end-element-handler))
                                   (comment . ,my-comment-handler)
                                   (cdata-section
                                    . (,my-start-cdata-section-handler
                                       ,my-end-cdata-section-handler))))))
         (lambda (key args)
           (err "Received error ") (err key)(err "\n"))))

(define (test-encoding-conv)
  (let ((from "an ASCII string"))
    (check (equal? (mixp:utf8->latin1 from) from)
           "mixp:utf8->latin1 on ASCII"
           (format #t "error with ascii string ~A != ~A\n"
                   (mixp:utf8->latin1 from) from)))
  (let ((from "BÃ©zecourt")(to "Bézecourt"))
    (check (equal? (mixp:utf8->latin1 from) to)
           "mixp:utf8->latin1 on Latin1"
           "error with Latin1 string\n"))
  ;; From rfc-2279
  (let ((from (map integer->char '(#x41 #xE2 #x89 #xA2 #xCE #x91 #x2E)))
        (to (map (lambda (c) (map integer->char c))
                 '((#x00 #x41) (#x22 #x62) (#x03 #x91) (#x00 #x2E)))))
    (check (equal? (mixp:utf8->ucs2 from) to)
           "mixp:utf8->ucs2"
           "error with rfc-2279 sample #1\n"))
  (let ((from (map integer->char
                   '(#xED #x95 #x9C #xEA #xB5 #xAD #xEC #x96 #xB4)))
        (to (map (lambda (c) (map integer->char c))
                 '((#xD5 #x5C) (#xAD #x6D) (#xC5 #xB4)))))
    (check (equal? (mixp:utf8->ucs2 from) to)
           "mixp:utf8->ucs2"
           "error with rfc-2279 sample #2\n"))
  (let ((from (map integer->char
                   '(#xE6 #x97 #xA5 #xE6 #x9C #xAC #xE8 #xAA #x9E)))
        (to (map (lambda (c) (map integer->char c))
                 '((#x65 #xE5) (#x67 #x2C) (#x8A #x9E)))))
    (check (equal? (mixp:utf8->ucs2 from) to)
           "mixp:utf8->ucs2"
           "error with rfc-2279 sample #3\n"))
  (check-exc (let ((from (map integer->char '(#xFF #xFF))))
               (mixp:utf8->ucs2 from))
             'invalid-utf8
             "mixp:utf8->ucs2 on invalid input"
             "An error was expected with invalid UTF-8 input (1)")
;;; Skipping a test on invalid UTF-8 input, because it is known to fail (see BUGS)
;;;  (catch 'invalid-utf8
;;;      (lambda ()
;;;        (let ((from (map integer->char
;;;                         '(#xC0 #x80))))
;;;          (mixp:utf8->ucs2 from)
;;;          (display "An error was expected with invalid UTF-8 input (2)\n")
;;;          (throw 'test-error))
;;;        )
;;;      (lambda (arg) '()))
  )

;; Test conversions from XML to lists or trees
(define (test-mixp->)
  (let ((document "<foo><bar>Some text</bar><void/></foo>"))
    (let ((xml-list (call-with-input-string document
                                            mixp:xml->list))
          (expected '((start-element "foo" ())
                      (start-element "bar" ())
                      (character-data "Some text")
                      (end-element "bar")
                      (start-element "void" ())
                      (end-element "void")
                      (end-element "foo"))))
      (check (equal? xml-list expected)
             "xml->list"
             (format #t "list built from XML different from expected list: ~A\n!=\n~A"
                     xml-list expected)))
    (let ((xml-tree (call-with-input-string document
                                            mixp:xml->tree))
          (expected '((element ("foo" ())
                        (element ("bar" ())
                          (character-data "Some text"))
                        (element ("void" ()))))))
      (check (equal? xml-tree expected)
             "xml->tree"
             (format #t
                     "tree built from XML different from expected tree ~A\n!=\n~A"
                     xml-tree expected))))
  (let* ((file "../samples/REC-xml-19980210.xml")
         (xml-list (call-with-input-file file mixp:xml->list))
         (xml-tree (call-with-input-file file mixp:xml->tree)))
    (check (equal? xml-list (mixp:tree->list xml-tree))
           "comparison of xml->tree->list and xml->list"
           "xml->tree->list conversion different from xml->list conversion")
    (check (equal? xml-tree (mixp:list->tree xml-list))
           "xml->list->tree and xml->tree"
           "xml->list->tree conversion different from xml->tree conversion")
    (check (equal? (list-ref (list-ref (list-ref
                                        (list-ref (car xml-tree) 5)
                                        19)
                                       32)
                             1)
                   '("div2" (("id" . "sec-predefined-ent"))))
           "check an element in the tree"
           "failed")
    (check (equal? (list-ref xml-list 2001)
                   '(start-element "nt" (("def" . "NT-Nmtoken"))))
           "check an element in a list"
           "failed")))

(define (test-dtd-parsing)
  (let ((my-document
         "<!DOCTYPE numbers SYSTEM \"test.dtd\">
<numbers>&numbers;</numbers>")
        (my-parser (expat:parser-create))
        (str ""))
    (expat:set-param-entity-parsing my-parser
                                    'expat:XML_PARAM_ENTITY_PARSING_ALWAYS)
    (expat:set-character-data-handler my-parser
                                      (lambda (data value)
                                        (set! str (string-append str
                                                                 value))))
    (expat:set-external-entity-ref-handler
     my-parser (lambda (my-parser
                        context
                        base
                        system-id
                        public-id)
                 (open-file system-id "r")))
    (let ((res-parse (expat:parse my-parser
                                  my-document
                                  #t)))
      (check (not (= res-parse 0))
             "parse"
             (format #t "Parser error: ~A(~A)"
                     (expat:get-error-code my-parser)
                     (expat:error-string (expat:get-error-code my-parser))))
      (check (equal? str "0, 2, 4, 6, 8, 1, 3, 5, 7, 9")
             "check the entity expansion"
             "failed"))))

(define (usage exit-code)
  (display "Usage: guile -s test-gexpat.scm [-d] [-h]\n")
  (quit exit-code))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main program : executes test functions, and catches the errors
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((errors-list '()))
  (let ((cmd-line (command-line)))
    (cond ((member "-d" cmd-line) (set! trace-level 10))
          ((member "-h" cmd-line) (usage 0))))
  (map (lambda (item)
         (let* ((failed-count-before (length test-failed))
                (fn (car item))
                (name (cadr item)))
           (newline)
           (display (string-append "* Test series: " name "\n"))
           (fn)
           (if (= (length test-failed) failed-count-before)
               (display "=> ok\n")
               (display (format #t "=> not ok: ~A tests failed\n"
                                (- (length test-failed)
                                   failed-count-before))))))
       (list
        (list test-error-string "Test the error string reporting")
        (list test-handlers  "Test handlers")
        (list test-not-well-formed "Try to parse a not well-formed document")
        (list test-unparsed-entities "Test unparsed entities and notations")
        (list test-encoding  "Create and print an encoding")
        (list test-unknown-encoding
              "Use a document with characters which have a wrong encoding")
        (list test-user-data "User data in the XML parser object")
        (list test-use-parser-arg
              "Receive the parser in the handlers, not the user data")
        (list test-base "Use the base")
                                        ;       (list test-encoding-parse "parse a document with a special encoding")
        (list test-encoding-conv "Test the encoding conversion functions")
        (list test-dtd-parsing "Test the DTD parsing feature")
        (list test-mixp-> "Test the XML to list and XML to tree conversions (may be long)")
        ))
  (newline)
  (if (= (length test-failed) 0)
      (begin
        (display "All tests are successful") (newline))
      (begin
        (display "The following tests failed: ") (newline)
        (for-each (lambda (t) (display (format #t "\t~A: ~A\n"
                                               (car t) (cadr t))))
                  (reverse test-failed)))))
