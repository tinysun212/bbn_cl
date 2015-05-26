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
(proclaim '(insert-touches nil))

(export '(
	  read-line read-char unread-char peek-char listen clear-input read-char-no-hang
	  read-byte write-string write-line write-char finish-output force-output clear-output
	  write-byte open close
          *terminal-io* *standard-input* *standard-output* *error-output* 
	  *query-io* *debug-io* *trace-output*
	  make-synonym-stream make-broadcast-stream make-concatenated-stream
	  make-two-way-stream make-echo-stream make-string-input-stream  make-string-output-stream
	  get-output-stream-string with-open-stream with-open-file with-input-from-string 
	  with-output-to-string streamp input-stream-p output-stream-p stream-element-type
	  file-position file-length
))

(defvar *terminal-io*)
(defvar *standard-input*)
(defvar *standard-output*)
(defvar *error-output*)
(defvar *query-io*)
(defvar *debug-io*)
(defvar *trace-output*)

;;;
;;; User-visible functions
;;;

(cl-define read-string                      (make-primitive-procedure 'cl-read-string))
(cl-define read-line                        (make-primitive-procedure 'cl-read-line))
(cl-define read-char                        (make-primitive-procedure 'cl-read-char))
(cl-define read-char-no-hang                (make-primitive-procedure 'cl-read-char-no-hang))
(cl-define unread-char                      (make-primitive-procedure 'cl-unread-char))
(cl-define peek-char                        (make-primitive-procedure 'cl-peek-char))
(cl-define listen                           (make-primitive-procedure 'cl-listen))
(cl-define clear-input                      (make-primitive-procedure 'cl-clear-input))
(cl-define read-byte                        (make-primitive-procedure 'cl-read-byte))
(cl-define write-string                     (make-primitive-procedure 'cl-write-string))
(cl-define internal-write-string            (make-primitive-procedure 'cl-internal-write-string))
(cl-define internal-write-string-start-end  (make-primitive-procedure 'cl-internal-write-string-start-end))
(cl-define write-line                       (make-primitive-procedure 'cl-write-line))
(cl-define write-char                       (make-primitive-procedure 'cl-write-char))
(cl-define finish-output                    (make-primitive-procedure 'cl-finish-output))
(cl-define force-output                     (make-primitive-procedure 'cl-force-output))
(cl-define clear-output                     (make-primitive-procedure 'cl-clear-output))
(cl-define write-byte                       (make-primitive-procedure 'cl-write-byte))
(cl-define close                            (make-primitive-procedure 'cl-close))
(cl-define file-position                    (make-primitive-procedure 'cl-file-position))
(cl-define file-length                      (make-primitive-procedure 'cl-file-length))
(cl-define get-output-stream-string         (make-primitive-procedure 'cl-get-output-stream-string))
(cl-define make-string-output-stream        (make-primitive-procedure 'cl-make-string-output-stream))
(cl-define make-string-input-stream         (make-primitive-procedure 'cl-make-string-input-stream))
(cl-define make-the-tty-stream              (make-primitive-procedure 'cl-make-the-tty-stream))
(cl-define make-synonym-stream              (make-primitive-procedure 'cl-make-synonym-stream))
(cl-define make-two-way-stream              (make-primitive-procedure 'cl-make-two-way-stream))
(cl-define make-echo-stream                 (make-primitive-procedure 'cl-make-echo-stream))
(cl-define charpos                          (make-primitive-procedure 'cl-charpos))
(cl-define stream-pathname                  (make-primitive-procedure 'cl-stream-pathname))
(cl-define string-input-stream-index        (make-primitive-procedure 'cl-string-input-stream-index))
(cl-define set-bfio-mode!                   (make-primitive-procedure 'cl-set-bfio-mode!))

;;;
;;; Debugging stuff
;;;

(cl-define system-vector-32b-ref    (make-primitive-procedure 'system-vector-32b-ref))
(cl-define system-vector-32b-length (make-primitive-procedure 'system-vector-32b-length))

(defun print-stream (s)
  (let ((*print-length* 50))
    (dotimes (i 6)
      (print-thing (system-vector-ref s i)))))

(defun print-thing (x &key print-string)
  (cond
   ((eq (object-type x) 'vector-32b)
    (dotimes (i (min 50 (system-vector-32b-length x)))
	     (print (system-vector-32b-ref x i))))
   ((eq (object-type x) 'vector)
    (dotimes (i (min 50 (vector-length x)))
	     (print-thing (vector-ref x i))))
   ((stringp x)
    (if print-string
	(print x)
	(progn
	 (terpri)
	 (princ #\")
	 (princ (substring x 0 5))
	 (princ "...")
	 (princ #\"))))
   (t (print x))))

;;;
;;; Predicates and inquiry functions
;;;

(cl-define stream-element-type (make-primitive-procedure 'cl-stream-element-type))
(cl-define stream-direction (make-primitive-procedure 'cl-stream-direction))

(defun streamp (s) (eq (object-type s) 'cl-stream))

(defun input-stream-p (s)
  (and (streamp s)
       (memq (stream-direction s) '(:input :io))))

(defun output-stream-p (s)
  (and (streamp s)
       (memq (stream-direction s) '(:output :io))))

;;;
;;; Stream-oriented macros (not used by stream system itself)
;;;

;;;  Bindspec is of the form (Stream File-Name . Options).  The file whose name
;;;  is File-Name is opened using the Options and bound to the variable Stream.
;;;  The Forms are executed, and when they terminate, normally or otherwise,
;;;  the file is closed.

(defmacro with-open-file ((var &rest open-args) &body (forms decls))
  (let ((abortp (gensym)))
    `(let ((,var (open ,@open-args))
	   (,abortp t))
       ,@decls
       (unwind-protect
	 (multiple-value-prog1
	  (progn ,@forms)
	  (setq ,abortp nil))
	 (when ,var
	   (close ,var :abort ,abortp))))))

(defmacro with-open-stream ((var stream) &body (forms decls))
  (let ((abortp (gensym)))
    `(let ((,var ,stream)
	   (,abortp t))
       ,@decls
       (unwind-protect
	 (multiple-value-prog1
	  (progn ,@forms)
	  (setq ,abortp nil))
	 (when ,var
	   (close ,var :abort ,abortp))))))

'$split-file

(cl-define string-open (make-primitive-procedure 'cl-string-open))

;;;  Binds the Var to an input stream that returns characters from String and
;;;  executes the body.

(defmacro with-input-from-string ((var string &key (start 0) (end `(length ,string)) index) &body (forms decls))
  (let ((res (gensym)))
    `(let ((,var (string-open ,string :input ,start ,end))
	   (,res nil))
       (unwind-protect
	(setq ,res (progn ,@forms))
	,(when index `(setf ,index (buf-stream-buf-index ,var)))
	(close ,var)
	,res))))

;;;  Binds the Var to a string output stream that puts characters into String
;;;  and executes the body.

(defmacro with-output-to-string ((var &optional string) &body (forms decls))
  (let ((res (gensym)))
    (if string
	`(let ((,var (string-open string :output 0 0)))
	   ,@decls
	   (unwind-protect
	    (progn 
	      ,@forms)
	    (close ,var)))
	`(let ((,var (string-open nil :output 0 0)))
	   ,@decls
	   (unwind-protect
	    (progn 
	      ,@forms
	      (get-output-stream-string ,var))
	    (close ,var))))))

;;;                             ********
;;;                             * Open *
;;;                             ********

;;;
;;; This table should track cl-fileio.h
;;;

(defconstant open-key-table
  '(
    (:input             . 0)
    (:output            . 1)
    (:io                . 2)
    (:error             . 3)
    (:create            . 4)
    (nil                . 5)
    (:new-version       . 6)
    (:rename            . 7)
    (:rename-and-delete . 8)
    (:overwrite         . 9)
    (:append            . 10)
    (:supersede         . 11)
    (:probe             . 12)
))

(cl-define file-open-fd     (make-primitive-procedure 'cl-file-open-fd))
(cl-define make-file-stream (make-primitive-procedure 'cl-make-file-stream))
(cl-define io-error-code-p  (make-primitive-procedure 'io-error-code-p))

(defun open (filename &key (direction :input) (element-type 'string-char)
		      (if-exists nil exists-p)
		      (if-does-not-exist nil does-not-exist-p))
  "Return a stream which reads from or writes to Filename.
  Defined keywords:
   :direction - one of :input, :output or :probe
   :element-type - Type of object to read or write, default String-Char
   :if-exists - one of :error, :new-version, :overwrite, :append or nil
   :if-does-not-exist - one of :error, :create or nil
  See the manual for details."
  (when (equal filename "console:") (return-from open (tty-open "console:")))
  (unless (memq direction '(:input :output :io :probe))
    (error "~S is an invalid direction for open." direction))
  (unless (memq if-exists '(:error :new-version :overwrite :append  nil))
    (error "~S is an invalid if-exists value for open." if-exists))
  (unless (memq if-does-not-exist '(:error :create nil))
    (error "~S is an invalid if-does-not-exist value for open." if-does-not-exist))
  (let* ((pathname (pathname filename))
	 (name (namestring pathname))
	 (version (pathname-version pathname))
	 (stream nil)
         (fd nil))
    ;;
    ;; Do hairy defaulting of :if-exists and :if-does-not-exist keywords.
    ;;
    (unless exists-p
      (setq if-exists (if (or (null version)
			      (eq version :newest))
			  :new-version 
			  :error)))
    (unless does-not-exist-p
      (setq if-does-not-exist
	    (cond
	     ((or (memq if-exists '(:overwrite :append)) (eq direction :input))
	      :error)
	     ((eq direction :probe) nil)
	     (t :create))))
    (setq fd (file-open-fd name 
			   (cdr (assq direction open-key-table))
			   (cdr (assq if-exists open-key-table))
			   (cdr (assq if-does-not-exist open-key-table))))
    (cond
     ((null fd) (return-from open nil))
     ((io-error-code-p fd) (error "Open error ~a; filename ~a~%" fd name)))
    (make-file-stream fd pathname (eq if-exists :append) direction 
		      (canonicalize-stream-element-type element-type))))

(defun canonicalize-stream-element-type (type-spec)
  (labels ((type-error () (error "Illegal binary-stream element-type ~a~%" type-spec)))
    (cond
     ((eq type-spec 'string-char)
      type-spec)
     ((not (subtypep type-spec 'integer))
      (type-error))
     ((memq type-spec '(signed-byte unsigned-byte))
      (list type-spec 8))
     ((eq (car type-spec) 'bit)
      '(unsigned-byte 1))
     (t (canonicalize-array-element-type type-spec)))))

;;;   *********************
;;;   * Broadcast Streams *
;;;   *********************

(cl-define %make-broadcast-stream (make-primitive-procedure 'cl-%make-broadcast-stream))

(defun make-broadcast-stream (&rest streams)
  (let ((element-types nil))
    (dolist (s streams)
	    (pushnew (stream-element-type s) 
		     element-types :test #'equal))
    (setq element-types
	  (cond
	   ((null element-types) nil)
	   ((= (length element-types) 1) (car element-types))
	   (t (cons 'and element-types))))
    (%make-broadcast-stream element-types (mapcar #'touch streams))))

;;;   ************************
;;;   * Concatenated Streams *
;;;   ************************

(cl-define %make-concatenated-stream (make-primitive-procedure 'cl-%make-concatenated-stream))

(defun make-concatenated-stream (&rest streams)
  (%make-concatenated-stream streams))

;;;
;;; Setup standard streams
;;;

(defvar *terminal-io*)
(setq *terminal-io* (make-the-tty-stream))

(defvar *standard-input*)
(defvar *standard-output*)
(defvar *error-output*)
(defvar *query-io*)
(defvar *debug-io*)
(defvar *trace-output*)

(let ((the-stream (make-synonym-stream '*terminal-io*)))
  (setq *standard-input* the-stream)
  (setq *standard-output* the-stream)
  (setq *error-output* the-stream)
  (setq *query-io* the-stream)
  (setq *debug-io* the-stream)
  (setq *trace-output* the-stream))

