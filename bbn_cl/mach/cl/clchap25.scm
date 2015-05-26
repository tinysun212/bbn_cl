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
;;;
;; Chapter 25 -- Miscellaneous Features

(proclaim '(insert-touches nil))

(export '(
	  documentation time internal-time-units-per-second
	  get-internal-run-time get-internal-real-time get-decoded-time
	  get-universal-time decode-universal-time encode-universal-time
	  compile compile-file trace untrace step describe inspect room
	  ed lisp-implementation-type lisp-implementation-version machine-type
	  machine-version machine-instance software-type software-version
	  short-site-name long-site-name dribble disassemble
	  sleep
))

(defvar *really-compile-procedures* t)

(defun compile (name &optional definition)
  (fluid-let ((*eval-when-mode* 'compiling))
    (setq definition (or definition (symbol-function name)))
    (let* ((lambda? (and (consp definition)
			 (or 
			  (eq (car definition) *user-lambda-symbol*)
			  (eq (car definition) 'lambda))))
	   (def (if lambda?
		    (eval definition)
		    definition)))
      (if (not (compound-procedure? def))
	  (error "Can't compile non-procedure ~s" definition))
      (newline)
      (princ ";;; Compiling ")
      (cond
       (name
	(progn
	  (princ "function ")
	  (princ name)))
       (lambda?
	(princ "(LAMBDA ")
	(princ (cadr definition))
	(princ " ..."))
       (else
	(princ def)))
      (let ((compiled-procedure
	     (if *really-compile-procedures*
		 (compile-procedure def 'touch #f 'noisy #f)
		 def)))
	(if name
	    (progn
	      (setf (symbol-function name) compiled-procedure)
	      ;; Set the internal definition of the defun
	      ;; See TRACE for more explanation.
	      (if (not (compiled-function-p def))
		  (lexical-assignment (procedure-environment def)
				      (fundefsym name)
				      compiled-procedure))
	    
	      name)
	    compiled-procedure)))))

(defun bin-file? (pathname)
  (call-with-input-file pathname
    (lambda (port)
      (= 250 (char->ascii (peek-char nil port))))))

(defun compile-file (input-file &key 
				(output-file nil)
				(info-files t)       ; may be nil (none), t (all), or '([rtl] [inf])
				(lisp-to-bin t)
				(bin-to-com t)
				(noisy t))
  ((access cl-compile-file cl-compile-file-package)
   input-file
   output-file
   noisy
   lisp-to-bin
   bin-to-com
   (or (eq info-files t)
       (memq :rtl info-files))
   (or (eq info-files t)
       (memq :inf info-files))))

(defun disassemble-file (filename)
  (let* ((pathname (pathname filename))
	 (brtl-pathname (merge-pathnames (make-pathname :type "brtl") pathname))
	 (com-pathname (merge-pathnames (make-pathname :type "com") pathname))
	 (*package* *bbnaci-package*))
    (if (file-exists? brtl-pathname)
	(progn
	  (format t "~%;;; Producing RTL file ~s" (merge-pathnames (make-pathname :type "rtl") pathname))
	  ((access compiler:write-rtl-file compiler-package) pathname))
	(format t "~%;;; File ~s does not exist. No RTL file produced." brtl-pathname))
    (if (file-exists? com-pathname)
	(progn
	  (format t "~%;;; Producing LAP file ~s" (merge-pathnames (make-pathname :type "lap") pathname))
	  (compiler:write-lap-file pathname))
	(format t "~%;;; File ~s does not exist. No LAP file produced." com-pathname))
    (terpri)))

(defun disassemble (symbol-or-proc)
  (let ((compiled-fcn 
	 (cond ((symbolp symbol-or-proc)
		(symbol-function symbol-or-proc))
	       ((compiled-function-p symbol-or-proc)
		symbol-or-proc)
	       (t (error "Sorry. Input to disassemble must be either a symbol or a compiled function object (~s)"
			 symbol-or-proc))))
	(*package* *bbnaci-package*))
    ((access disassembler/write-compiled-code-block disassembler-package compiler-package)
     (compiled-code-address->block (system-pair-car compiled-fcn))
     nil)))

(defun documentation (symbol doc-type)
  "Returns the documentation string of Doc-Type for the Symbol, or NIL if
  none exists.  System doc-types are VARIABLE, FUNCTION, STRUCTURE, TYPE,
  and SETF."
  (cond ((eq doc-type 'variable)
	 (system-get symbol '%var-documentation))
	((eq doc-type 'function)
	 (system-get symbol '%fun-documentation))
	((eq doc-type 'structure)
	 (system-get symbol '%struct-documentation))
	((eq doc-type 'type)
	 (system-get symbol '%type-documentation))
	((eq doc-type 'setf)
	 (system-get symbol '%setf-documentation))
	(t (cdr (assoc doc-type (system-get symbol '%documentation))))))

(defun %set-documentation (symbol doc-type string)
  (cond ((eq doc-type 'variable)
	 (%system-put symbol '%var-documentation string))
	((eq doc-type 'function)
	 (%system-put symbol '%fun-documentation string))
	((eq doc-type 'structure)
	 (%system-put symbol '%struct-documentation string))
	((eq doc-type 'type)
	 (%system-put symbol '%type-documentation string))
	((eq doc-type 'setf)
	 (%system-put symbol '%setf-documentation string))
	(t (let ((pair (assoc doc-type (system-get symbol '%documentation))))
	     (if pair (rplacd pair string)
		 (push (cons doc-type string) (system-get symbol '%documentation))))))
  string)

'$split-file

(cl-define get-internal-run-time (make-primitive-procedure 'get-internal-run-time))

(defconstant internal-time-units-per-second ((make-primitive-procedure 'get-internal-time-units-per-second)))

(defconstant scheme-internal-time-units-per-second 1000)

(defun get-internal-real-time ()
  (* internal-time-units-per-second 
     (round (real-time-clock)
	    scheme-internal-time-units-per-second)))

(defconstant cl-gc-keys '(:quiet :bell :brief :verbose))
(defconstant gc-key-map
  '(
    (:quiet   . quiet)
    (:bell    . bell)
    (:brief   . brief)
    (:verbose . verbose)
))

(defun cl-gc-key->scheme-gc-key (key)
  (let ((entry (assoc key gc-key-map)))
    (if entry
	(cdr entry)
	(error "Illegal gc notification keyword ~s" key))))

(defun scheme-gc-key->cl-gc-key (key)
  (car (rassoc key gc-key-map)))

(defun toggle-gc-notification ()
  (scheme-gc-key->cl-gc-key (toggle-gc-notification!)))

(defun gc-notification-mode (&optional (new-mode nil new-mode-supplied-p))
  (if (not new-mode-supplied-p)
      (values (scheme-gc-key->cl-gc-key (car ((no-fundefsym gc-notification-mode))))
	      cl-gc-keys)
      (values (scheme-gc-key->cl-gc-key ((no-fundefsym gc-notification-mode)
					 (cl-gc-key->scheme-gc-key new-mode)))
	      cl-gc-keys)))

(defmacro time (e)
  `(time-thunk #'(lambda () (touch ,e))))

(cl-define unix-ctime (make-primitive-procedure 'unix-ctime))
(cl-define get-time-zone (make-primitive-procedure 'get-time-zone))
(cl-define get-universal-time (make-primitive-procedure 'get-universal-time))
(cl-define unix-inverse-ctime (make-primitive-procedure 'unix-inverse-ctime))

(defun get-decoded-time ()
  (decode-universal-time (get-universal-time)))

(defun decode-universal-time (universal-time &optional (time-zone nil tz-supplied?))
  (setq universal-time (touch universal-time))
  (setq time-zone (touch time-zone))
  (let ((time-list (vector->list (unix-ctime universal-time)))
	)
    (if tz-supplied?
    	(set-car! (last-pair time-list) time-zone))
    (values-list time-list)))

;; Time-zone is always a valid time-zone.  Whether or not to adjust
;; for daylight savings time depends on whether optional time-zone was
;; supplied.  If it is, then adjusting for dst is not done.
;;
(defun encode-universal-time (second minute hour date month year &optional (time-zone (get-time-zone) tz-supplied))
  (declare (insert-touches t))
  (let ((time-vector ((access vector nil) second minute hour date month year time-zone))
	)
    (unix-inverse-ctime time-vector tz-supplied)))

;;;; Debugging Tools, section 25.3


;;; scheme's trace is useful, too.

(cl-define scm-trace trace)
(cl-define scm-untrace untrace)

(define scheme-trace trace)
(define scheme-untrace untrace)

(defvar *traced-functions*)
(setq *traced-functions* '())

;; There is some considerable hair here because of what we emit for
;; defuns.  Namely, a defun turns into a
;; (defun-intern <name> (let () (define (<name> ...) ...) <name>))
;; Thus, just setting the function symbol is not good enough, because
;; the internal define resolves any internal calls directly to itself
;; through the let environment.  Thus, for interpreted procedures, besides
;; bashing the function-cell, we also have to bash the internal-environment
;; of the let.  For compiled procedures, we lose, as any recursive calls turn
;; directly into a branches.  This could be simplified if we change
;; what is emitted for defun.  Jinx says we might do this if the new
;; compiler with its good uuo-links makes this pratical.  Note that this
;; same problem exists with COMPILE.

(defmacro trace (&rest procs)
  (if (null procs)
      `(quote ,(mapcar #'car *traced-functions*))
      (dolist (f procs)
	(if (assoc f *traced-functions*)
	    (warn "~S is already traced." f)
	    (let ((real-function (symbol-function f)))
	      (setq *traced-functions*
		    (acons f real-function *traced-functions*))
	      (let ((traced-function 
		     #'(lambda (&rest args)
			 (format *trace-output* "~%;;; [Entering (~A~{ ~S~})]" f args)
			 (let ((returned-values
				(multiple-value-list (apply real-function args))))
			   (let ((n (length returned-values)))
			     (cond ((zerop n)
				    (format *trace-output*
					    "~%;;; [No values were returned by (~A~{ ~S~})]"
					    f
					    args))
				   ((= 1 n)
				    (format *trace-output*
					    "~%;;; [~S was returned by (~A~{ ~S~})]"
					    (car returned-values)
					    f
					    args))
				   (else
				    (format *trace-output*
					    "~%;;; [~{~S ~}were returned by (~A~{ ~S~})]"
					    returned-values
					    f
					    args))))
			   (values-list returned-values)))))
		(setf (symbol-function f) traced-function)
		(if (not (compiled-function-p real-function))
		    (lexical-assignment (procedure-environment real-function)
					(fundefsym f)
					traced-function))))))))

(defmacro untrace (&rest procs)
  (if (null procs)
      (setq procs (mapcar #'car *traced-functions*)))
  (dolist (f procs)
    (let ((real-function (cdr (assoc f *traced-functions*))))
      (if (null? real-function)
	  (warn "~S was not being traced." f)
	  (progn
	    (setf (symbol-function f) real-function)
	    (if (not (compiled-function-p real-function))
		(lexical-assignment (procedure-environment real-function)
				    (fundefsym f)
				    real-function))
	    (setq *traced-functions*
		  (remove-if #'(lambda (el) (eq (car el) f))
			     *traced-functions*)))))))

;; Eventually, we will have to use Scheme's stepper facilities (AJC knows
;; about this)

(defmacro step (form)
  `(commonlisp-nyi 'step))

(defun describe (object)
  (if (symbolp object)
      (let ((boundp (boundp object))
	    (fboundp (fboundp object)))
	(let ((value (if boundp (symbol-value object)))
	      (fvalue (if fboundp (symbol-function object))))
	  (format t "Symbol: ~s, ~:[~*~;value: ~s, ~]~:[~*~;function: ~s, ~]plist: ~s"
		  object boundp value fboundp fvalue (symbol-plist object))))
      (pp object))   ; touching is on in the pp
  (values))

(defun inspect (object)
  (commonlisp-nyi 'inspect))

(defun room (&optional x)
  (print-gc-statistics))

(defun ed (&optional x)
  (commonlisp-nyi 'ed))

;; Dribble

;;; How do you make this saving of the io-stream work on a multiprocessor?

(defvar *dribble-saved-terminal-io-stream* '())
(defvar *dribble-output-stream* '())

(defun dribble (&optional pathname)
  (setq pathname (touch pathname))
  (if pathname
      (let ((f (open pathname :direction :output  :if-exists :new-version
		     :if-does-not-exist :create)))
	(if *dribble-output-stream*
	    (progn (format t ";;; Closing previous dribble stream ~s ...~2%" *dribble-output-stream*)
		   (close *dribble-output-stream*)
		   (setq *terminal-io* *dribble-saved-terminal-io-stream*)))
	(setq *dribble-saved-terminal-io-stream* *terminal-io*
	      *dribble-output-stream*            f
	      *terminal-io*                      (make-two-way-stream
						  (make-echo-stream *terminal-io* f)
						  (make-broadcast-stream *terminal-io* f)))
	(format t ";;; Dribbling to ~s~%" f)
	(values))
      (if (null *dribble-saved-terminal-io-stream*)
	  (values)
	  (progn
	   (format t ";;; Stopping dribble to ~s~%" *dribble-output-stream*)
	   (close *dribble-output-stream*)
	   (setq *terminal-io*                      *dribble-saved-terminal-io-stream*
		 *dribble-output-stream*            '()
		 *dribble-saved-terminal-io-stream* '())
	   (values)))))

;; Apropos & apropos-list are in package.scm

(cl-define cl-sleep (make-primitive-procedure 'cl-sleep))

(defun sleep (seconds)
  (setq seconds (round seconds))
  (if (minusp seconds)
      (error "Argument to seconds must be non-negative ~d"
	     seconds)
      (cl-sleep seconds)))
