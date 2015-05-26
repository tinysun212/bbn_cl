;;;                                ********
;;;
;;; Copyright 1992 by BBN Systems and Technologies, A division of Bolt,
;;; Beranek and Newman Inc.
;;; 
;;; Permission to use, copy, modify and distribute this software and its
;;; documentation is hereby granted without fee, provided that the above
;;; copyright notice and this permission appear in all copies and in
;;; supporting documentation, and that the name Bolt, Beranek and Newman
;;; Inc. not be used in advertising or publicity pertaining to distribution
;;; of the software without specific, written prior permission.  In
;;; addition, BBN makes no respresentation about the suitability of this
;;; software for any purposes.  It is provided "AS IS" without express or
;;; implied warranties including (but not limited to) all implied warranties
;;; of merchantability and fitness.  In no event shall BBN be liable for any
;;; special, indirect or consequential damages whatsoever resulting from
;;; loss of use, data or profits, whether in an action of contract,
;;; negligence or other tortuous action, arising out of or in connection
;;; with the use or performance of this software.
;;; 
;;;                                 ********
;;; 
;;;;; This is the root from which we bootstrap Butterfly Common Lisp from its
;; unlikely Scheme origins.
;; ["kernel" is really a misnomer now -- it's really the "big switch" file]

(declare (usual-integrations))

;;;
;;; Controlling all the CommonLisp modes
;;;

;;; These "keywords" are not standard because we don't want them to be picked up 
;;;  by the cl package system later.

(define -name-mutation                      '-name-mutation)
(define -cl-read-file                       '-cl-read-file)
(define -ucode-cl-mode                      '-ucode-cl-mode)
(define -cl-parser-table                    '-cl-parser-table)     ; the boot parser extensions 
(define -cl-syntax-table                    '-cl-syntax-table)
(define -sf-cl-syntax-table                 '-sf-cl-syntax-table)
(define -cl-io                              '-cl-io)               ; whether to use cl read, print, and streams
(define -cl-prompt                          '-cl-prompt)

;;;
;;; (cl-mode [ {#t #f t} | <name> <value> ... ])
;;;
;;;  No args will show modes.
;;;  True will set all, but will process remainder to specifically
;;;   set or unset those desired.
;;;  So, eg., (cl-mode #t -cl-ucode-mode #f) will set all modes except the ucode mode.
;;;

(define cl-mode
  (let ((scheme-prompt *rep-current-prompt*))
    (define (print-info-line name info n-tabs #!optional info-printer)
      (if (unassigned? info-printer)
	  (set! info-printer princ))
      (princ "\t")
      (princ name)
      (let loop ((n n-tabs)) (if (zero? n) () (begin (princ "\t") (loop (-1+ n)))))
      (info-printer info)
      (newline))
    (define (cl-mode-aux settings)
      (if (null? settings)
	  #f
	  (let ((name (car settings))
		(value (cadr settings)))
	    (case name
	      (-name-mutation      (set! *get-fcn-name* (if value fundefsym (lambda (x) x))))
	      (-cl-read-file       (set! *read-file* (if value cl-read-file read-file)))
	      (-ucode-cl-mode      (vector-set! (get-fixed-objects-vector) #x29 value))
	      (-cl-parser-table    (if value
				       (set! *parser-table* *commonlisp-parser-table*)
				       (set! *parser-table* *scheme-parser-table*)))
	      (-sf-cl-syntax-table (set! *get-default-syntax-table*
					 (if value
					     cl-get-sf-syntax-table
					     (lambda () *default-global-syntax-table*))))
	      (-cl-syntax-table    (if value
				       (set-rep-base-syntax-table! *commonlisp-syntax-table*)
				       (set-rep-base-syntax-table! *scheme-syntax-table*)))
	      (-cl-prompt         (if value
				      (set-rep-base-prompt! cl-prompt)
				      (set-rep-base-prompt! scheme-prompt)))
	      (-cl-io             (cl-io-mode value))
	      )
	    (cl-mode-aux (cddr settings)))))
    (named-lambda (cl-mode . settings)
      (if (null? settings)
	  (begin
	    (princ "(cl-mode")
	    (newline)
	    (print-info-line -name-mutation           (eq? *get-fcn-name* fundefsym) 2)
	    (print-info-line -cl-read-file            (eq? *read-file* cl-read-file) 2)
	    (print-info-line -ucode-cl-mode           (vector-ref (get-fixed-objects-vector) #x29) 2)
	    (print-info-line -cl-parser-table         (eq? *parser-table* *commonlisp-parser-table*) 1)
	    (print-info-line -cl-syntax-table         (eq? *rep-current-syntax-table* *commonlisp-syntax-table*) 1)
	    (print-info-line -sf-cl-syntax-table      (eq? *get-default-syntax-table* cl-get-sf-syntax-table) 1)
	    (print-info-line -cl-io                   (cl-io-mode 'inquire) 3)
	    (print-info-line -cl-prompt               (eq? *rep-current-prompt* cl-prompt) 2)
	    (princ ")")
	    (newline))
	  (cl-mode-aux (gather-settings settings))))))

(define (gather-settings l)
  (let loop ((l l) (s '()))
    (cond
     ((null? l) s)
     ((memq (car l) '(#f #t t))
      (let ((value (car l)))
	(loop (append `(-name-mutation ,value -cl-read-file ,value -sf-cl-syntax-table ,value
				       -ucode-cl-mode ,value -cl-parser-table ,value -cl-syntax-table ,value
				       -cl-io ,value -cl-prompt ,value)
		       (cdr l))
	      s)))
     (else
      (set! s (set-setting! s (car l) (cadr l)))
      (loop (cddr l) s)))))

(define (set-setting! s m v)
  (let loop ((l s))
    (cond
     ((null? l)
      (cons m (cons v s)))
     ((eq? m (car l))
      (set-car! (cdr l) v)
      s)
     (else
      (loop (cddr l))))))

'$split-file

(define cl-io-mode
  (let ((read-save read)
	(rep-read-hook-save rep-read-hook)
	(read-string-save read-string)
	(read-char-save read-char)
	(peek-char-save peek-char)
	(print-save print)
	(display-save display)
	(write-line-save write-line)
	(write-save write)
	(princ-save princ)
	(prin1-save prin1)
	(write-string-save write-string)
	(write-char-save write-char)
	(newline-save newline)
	(with-output-to-string-save with-output-to-string)
	(with-output-to-truncated-string-save with-output-to-truncated-string)
	(with-output-to-file-save with-output-to-file)
	(open-input-file-save open-input-file)
	(open-output-file-save open-output-file)
	(close-input-port-save close-input-port)
	(close-output-port-save close-output-port)
	(rep-output-port-save rep-output-port)
	(pp-flush-output-save (access flush-output scheme-pretty-printer))
	(current-output-port-save *current-output-port*)
	(rep-base-output-port-save *rep-base-output-port*)
	(clear-input-save clear-input)
	(pp-*unparse-char-save (access *unparse-char scheme-pretty-printer))
	(pp-*unparse-string-save (access *unparse-string scheme-pretty-printer))
	(with-output-to-port-save with-output-to-port)
	(output-port?-save output-port?))
    (define (peek-char-interface #!optional stream)
      (if (unassigned? stream) (set! stream *standard-input*))
      ((symbol-function 'peek-char) #f stream))
    (define (open-input-file-interface filename)
      ((symbol-function 'open) filename))
    (define (open-output-file-interface filename)
      ((symbol-function 'open) filename 
			       (keyword-intern "DIRECTION")
			       (keyword-intern "OUTPUT")))
    (define (write-interface object #!optional stream)
      (if (unassigned? stream) (set! stream *standard-output*))
      ((symbol-function 'write) object (keyword-intern "STREAM") stream)
      *the-non-printing-object*)
    (define (print-interface object #!optional stream)
      (if (unassigned? stream) (set! stream *standard-output*))
      ((symbol-function 'print) object stream)
      *the-non-printing-object*)
    (define (princ-interface object #!optional stream)
      (if (unassigned? stream) (set! stream *standard-output*))
      ((symbol-function 'princ) object stream)
      *the-non-printing-object*)
    (define (prin1-interface object #!optional stream)
      (if (unassigned? stream) (set! stream *standard-output*))
      ((symbol-function 'prin1) object stream)
      *the-non-printing-object*)
    (define (write-string-interface object #!optional stream)
      (if (unassigned? stream) (set! stream *standard-output*))
      ((symbol-function 'write-string) object stream)
      *the-non-printing-object*)
    (define (write-char-interface object #!optional stream)
      (if (unassigned? stream) (set! stream *standard-output*))
      ((symbol-function 'write-char) object stream)
      *the-non-printing-object*)
    (define (terpri-interface #!optional stream)
      (if (unassigned? stream) (set! stream *standard-output*))
      ((symbol-function 'terpri) stream)
      *the-non-printing-object*)      
    (define (read-interface #!optional stream)
      (if (unassigned? stream) (set! stream *standard-input*))
      ((symbol-function 'read) stream #f eof-object))
    (define (with-output-to-string-interface thunk)
      (fluid-let ((*standard-output* ((symbol-function 'make-string-output-stream))))
	(thunk)
	((symbol-function 'get-output-stream-string) *standard-output*)))
    (define (with-output-to-port-interface port thunk)
      (if (not (output-port? port)) (error "Bad output port" port))
      (fluid-let ((*standard-output* port))
	(thunk)))
    (define (with-output-to-truncated-string-interface max thunk)
      (fluid-let ((*standard-output* ((symbol-function 'make-string-output-stream))))
	(thunk)
	(let ((s ((symbol-function 'get-output-stream-string) *standard-output*)))
	  (if (> max (string-length s))
	      (cons #f s)
	      (cons #t (substring s 0 max))))))
    (define (rep-output-port-interface)
      *terminal-io*)
    (define (with-output-to-file-interface output-specifier thunk)
      (define new-port (open-output-file output-specifier))
      (define old-port)
      (dynamic-wind (lambda ()
		      (set! old-port
			    (set! *standard-output*
				  (set! new-port))))
		    thunk
		    (lambda ()
		      (let ((port))
			;; Only SET! is guaranteed to do the right thing with
			;; an unassigned value.  Binding may not work right.
			(set! port (set! *standard-output* (set! old-port)))
			(if (not (unassigned? port))
			    (close-output-port port))))))
    (lambda (value)
      (if (eq? value 'inquire)
	  (eq? read read-interface)
	  (begin
	    (set-transcript-hooks value)
	    (if value
		(begin
		  (set! read read-interface)
		  (set! rep-read-hook read)
		  (set! read-string (symbol-function 'read-string))
		  (set! read-char (symbol-function 'read-char))
		  (set! peek-char peek-char-interface)
		  (set! print print-interface)
		  (set! write-line print)
		  (set! write write-interface)
		  (set! princ princ-interface)
		  (set! display princ)
		  (set! prin1 prin1-interface)
		  (set! write-string write-string-interface)
		  (set! write-char write-char-interface)
		  (set! newline terpri-interface)
		  (local-assignment system-global-environment (fundefsym 'newline) newline)
		  (set! with-output-to-string with-output-to-string-interface)
		  (set! with-output-to-truncated-string with-output-to-truncated-string-interface)
		  (set! with-output-to-file with-output-to-file-interface)
		  (set! open-input-file open-input-file-interface)
		  (set! open-output-file open-output-file-interface)
		  (set! close-input-port (symbol-function 'close))
		  (set! close-output-port (symbol-function 'close))
		  (set! rep-output-port rep-output-port-interface)
		  (set! (access flush-output scheme-pretty-printer) (symbol-function 'force-output))
		  (set! *current-output-port* *standard-output*)
		  (set! *rep-base-output-port* *current-output-port*)
		  (set! clear-input (symbol-function 'clear-input))
		  (set! (access *unparse-char scheme-pretty-printer) (symbol-function 'internal-write-char))
		  (set! (access *unparse-string scheme-pretty-printer)
			(let ((internal-write-string (symbol-function 'internal-write-string)))
			  (lambda (string) (internal-write-string string #f))))
		  (set! with-output-to-port with-output-to-port-interface)
		  (set! output-port? (symbol-function 'streamp))
		  (set! (access trace-output-port advice-package) ((symbol-function 'make-synonym-stream)
								   '*trace-output*)))
		(begin
		  (set! read read-save)
		  (set! rep-read-hook rep-read-hook-save)
		  (set! read-string read-string-save)
		  (set! read-char read-char-save)
		  (set! peek-char peek-char-save)
		  (set! print print-save)
		  (set! display display-save)
		  (set! write-line write-line-save)
		  (set! write write-save)
		  (set! princ princ-save)
		  (set! prin1 prin1-save)
		  (set! write-string write-string-save)
		  (set! write-char write-char-save)
		  (set! newline newline-save)
		  (local-assignment system-global-environment (fundefsym 'newline) newline)
		  (set! with-output-to-string with-output-to-string-save)
		  (set! with-output-to-truncated-string with-output-to-truncated-string-save)
		  (set! with-output-to-file with-output-to-file-save)
		  (set! open-input-file open-input-file-save)
		  (set! open-output-file open-output-file-save)
		  (set! close-input-port close-input-port-save)
		  (set! close-output-port close-output-port-save)
		  (set! rep-output-port rep-output-port-save)
		  (set! (access flush-output scheme-pretty-printer) pp-flush-output-save)
		  (set! *current-output-port* current-output-port-save)
		  (set! *rep-base-output-port* rep-base-output-port-save)
		  (set! clear-input clear-input-save)
		  (set! (access *unparse-char scheme-pretty-printer) pp-*unparse-char-save)
		  (set! (access *unparse-string scheme-pretty-printer) pp-*unparse-string-save)
		  (set! with-output-to-port with-output-to-port-save)
		  (set! output-port? output-port?-save)
		  (set! (access trace-output-port advice-package) '()))))))))

'$split-file  ; just to avoid out-of-memory abort

(define (cl-get-sf-syntax-table) *rep-current-syntax-table*)

(define (cl-prompt)
  (newline)
  (newline)
  (write-string (package-name *package*))
  (write-string "]=> "))

;;;
;;; Easy ways to get to Scheme from Common Lisp and vice-versa.
;;;

(define enter-commonlisp)
(define enter-scheme)

(let ((saved-package *package*)
      (saved-env *rep-current-environment*))
  (set! enter-commonlisp
	(lambda ()
	  (set! *process-declarations* #t)
	  (set! saved-env *rep-current-environment*)
	  (cl-mode #t)
	  (%ge *commonlisp-user-environment*)
	  ((eval (fundefsym 'in-package) '()) (package-name saved-package))
	  (newline)
	  (princ "*** You are now in Common Lisp, talking to the ")
	  (princ (package-name saved-package))
	  (princ " package ***")
	  #f))
  (set! enter-scheme
	(lambda ()
	  (set! saved-package *package*)
	  (set! *process-declarations* #f)
	  ((eval (fundefsym 'in-package) '()) 'bbnaci)
	  (cl-mode #f)
	  (%ge saved-env)
	  (newline)
	  (princ "*** You are now in Scheme, talking to ")
	  (print-env saved-env)
	  (princ " ***")
	  (newline)
	  #f)))

(define (print-env env)
  (princ
   (cond
    ((eq? env system-global-environment)
     "the system-global-environment")
    ((eq? env user-initial-environment)
     "the user-initial-environment")
    (else
     (write-to-string env)))))
	  
;;;
;;; Commonlisp history variables
;;;

(define ++ '())
(define +++ '())
(define ** '())
(define *** '())
(define // '())
(define /// '())

(define (cl-rep-transcript-read-hook expression)
  (let ((e *commonlisp-user-environment*))
    (set! +++ ++)
    (set! ++ (lexical-reference e '+))
    (local-assignment e '+ (lexical-reference e '-))
    (local-assignment e '- expression)))

(define (cl-rep-transcript-value-hook values-list)
  (let ((e *commonlisp-user-environment*))
    (set! /// //)
    (set! // (lexical-reference e '/))
    (local-assignment e '/ values-list)
    (set! *** (if /// (car ///) '()))
    (set! ** (if // (car //) '()))
    (local-assignment e '* (if (lexical-reference e '/) 
			       (car (lexical-reference e '/))
			       '()))))

(define set-transcript-hooks)

(let ((rep-transcript-read-hook-save rep-transcript-read-hook)
      (rep-transcript-value-hook-save rep-transcript-value-hook))
  (set! set-transcript-hooks
	(lambda (value)
	  (if value
	      (begin
		(set! rep-transcript-read-hook cl-rep-transcript-read-hook)
		(set! rep-transcript-value-hook cl-rep-transcript-value-hook)
		(let ((e *commonlisp-user-environment*))
		  (local-assignment e '+ '())
		  (local-assignment e '- '())
		  (local-assignment e '* '())
		  (local-assignment e '/ '())))
	      (begin
		(set! rep-transcript-read-hook rep-transcript-read-hook-save)
		(set! rep-transcript-value-hook rep-transcript-value-hook-save))))))

;;;
;;; Unparser hooks to print g-vectors, etc., in a readable manner;
;;;  not needed when cl printer is installed.
;;;

(in-package unparser-package

  (define-type 'g-vector
    (let ()
      (define st (make-primitive-procedure 'get-g-vector-subtype))
      (define glen (make-primitive-procedure 'g-vector-length))
      (define gref (make-primitive-procedure 'g-vector-ref))
      (lambda (x)
	(case (st x)
	  (0 (*unparse-string "#gv-gen-vec("))
	  (1 (*unparse-string "#gv-struct(")))
	(let ((l (glen x)))
	  (let loop ((n 0))
	    (if (= n l)
		#f
		(begin
		  (*unparse-object (gref x n))
		  (*unparse-char (if (= n (- l 1)) #\) #\space))
		  (loop (+ n 1)))))))))

  ;; A little help for common lisp generic arithmetic

  (define-type 'RATIO
    (lambda (rat)
      (*unparse-object ((symbol-function 'numerator) rat))
      (*unparse-char #\/)
      (*unparse-object ((symbol-function 'denominator) rat))))

  (define-type 'COMPLEX
    (lambda (c)
      (*unparse-object ((symbol-function 'realpart) c))
      (if (not ((symbol-function 'minusp) ((symbol-function 'imagpart) c)))
	  (*unparse-char #\+))
      (*unparse-object ((symbol-function 'imagpart) c))
      (*unparse-char #\i)))

)

'$split-file

;;;
;;; CommonLisp parser extensions
;;;

(define *scheme-parser-table* *parser-table*)
(define *commonlisp-parser-table* (parser-table-copy *parser-table*))

(in-package parser-package

  ;; #'

  (define (parse-dispatch-quote)
    (discard-char)
      (list 'function (parse-object)))

  (define parse-dispatch-quote-in-list
    (collect-list-wrapper
     parse-dispatch-quote))

  (set-parser-table-entry! *commonlisp-parser-table* "#'"
			   (cons parse-dispatch-quote parse-dispatch-quote-in-list))

  ;; :

  (define parse-leading-colon
    (lambda ()
      (discard-char)
      (if (char-set-member? atom-constituents (peek-char))
	  (let ((s (read-atom)))
	    (string-upcase! s)
	    (keyword-intern s))
	  (build-atom (string-append ":") (read-atom)))))

  (define parse-leading-colon-in-list
    (collect-list-wrapper parse-leading-colon))

  (set-parser-table-entry! *commonlisp-parser-table* #\:
    (cons parse-leading-colon parse-leading-colon-in-list))

  )

;;;
;;;  Extensions for floating number printing
;;;

(define *floating-zero* ((make-primitive-procedure 'coerce-integer-to-flonum) 0))

(define float-infinity? (make-primitive-procedure 'float-infinity?))

(define floating-infinity-printed (string->list "#[FLOATING-INFINITY]"))

(define float-nan? (make-primitive-procedure 'float-nan?))

(define floating-nan-printed (string->list "#[FLOATING-NAN]"))

(set! *special-float-unparser-hook*
      (lambda (x)
	(cond
	 ((= x *floating-zero*) '(#\0 #\.))
	 ((float-infinity? x) floating-infinity-printed)
	 ((float-nan? x) floating-nan-printed)
	 (else '()))))

;;;
;;; Add to the set of character names (these names come mainly from Spice)
;;;

(let loop ((entries `(
		      ("Null" . 0)
		      ("Bell" . 7)
		      ("BS" . 8)
		      ("Backspace" . 8)
		      ("Tab" . 9)
		      ("FF" . 12)
		      ("Formfeed" . 12)
		      ("Page" . 12)
		      ("CR" . 13)
		      ("Return" . 13)
		      ("Newline" . 10)
		      ("Altmode" . 27)
		      ("Alt" . 27)
		      ("Esc" . 27)
		      ("Escape" . 27)
		      ("Sp" . 32)
		      ("Space" . 32)
		      ("Delete" . 127)
		      ("Rubout" . 127)
		      )))
  (if (null? entries)
      #f
      (begin
	(char:add-named-code! (car (car entries)) (cdr (car entries)))
	(loop (cdr entries)))))

;;;
;;; Alter float reader to accept the full cl set
;;; of float exponent indicators
;;;

(in-package number-parser-package
  (define (parse-suffix chars receiver)
    (if (and (not (null? chars))
	     (char-ci-member? (car chars) '(#\s #\f #\e #\d #\l)))
	(parse-signed-suffix (cdr chars) receiver)
	(receiver chars false)))
  (define (char-ci-member? c l)
    (if (null? l) 
	#f
	(or
	 (char-ci=? c (car l))
	 (char-ci-member? c (cdr l)))))
)

;;;
;;; Change some of the error messages that come from scheme to make them
;;;  more common-lispy
;;;

;;; Temp for now so that errors in the build will work

(define *standard-output* '())
(define *standard-input* '())
(define *error-output* '())
(define *debug-io* '())

(in-package error-system

  (let ((rep-error-hook *rep-error-hook*))
    (set! *rep-error-hook* 
	  (lambda (environment message irritant substitute-environment?)
	    (if (and (not (unassigned? *error-code*))
		     (= *error-code* (microcode-error 'unassigned-variable))
		     (%function-symbol%? irritant))
		(fluid-let ((*standard-output* *error-output*))
		  (newline)
		  (princ ";;; Error: Undefined function ")
		  (princ (unfundefsym irritant))
		  (fluid-let ((*standard-input* *debug-io*)
			      (*standard-output* *debug-io*))
		    (rep-error-hook environment "" *the-non-printing-object* substitute-environment?)))
		(fluid-let ((*standard-output* *error-output*))
		  (if (not (and (string=? message "")
				(eq? irritant *the-non-printing-object*)))
		      (begin
			(newline)
			(princ ";;; Error: ")
			(princ message)
			(princ " ")
			(princ irritant)))
		  (fluid-let ((*standard-input* *debug-io*)
			      (*standard-output* *debug-io*))
		    (rep-error-hook environment "" *the-non-printing-object* substitute-environment?)))))))
)

(define *commonlisp-user-environment*
  (let ()
    (the-environment)))

