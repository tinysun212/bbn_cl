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

;;; Toplevel print functions

(export '(*print-escape* *print-pretty* *print-circle* *print-base* *print-radix*
	  *print-case* *print-level* *print-length* *print-array* *print-gensym*
	  write prin1 print pprint princ write-to-string prin1-to-string
	  princ-to-string terpri fresh-line format y-or-n-p yes-or-no-p))

;;; Top level control variables

(defvar *print-escape*)
(setq *print-escape* T)

(defvar *print-pretty*)
(setq *print-pretty* ())

(defvar *print-base*)
(setq *print-base* 10)

(defvar *print-radix*)
(setq *print-radix* '())

(defvar *print-level*)
(setq *print-level* '())

(defvar *print-length*)
(setq *print-length* '())

(defvar *print-circle*)
(setq *print-circle* '())

(defvar *print-case*)
(setq *print-case* :UPCASE)

(defvar *print-array*)
(setq *print-array* T)

(defvar *print-gensym*)
(setq *print-gensym* T)

(set! (access *put-hash* cl-unparser-package)
      (lambda (key value table)
	(setf (gethash key table) value)))
(set! (access *get-hash* cl-unparser-package)
      #'gethash)
(set! (access *get-hash-count* cl-unparser-package)
      #'hash-table-count)
(set! (access *make-eq-hash-table* cl-unparser-package)
      (lambda () (make-hash-table :test 'eq)))
(set! (access *bit-vector-p* cl-unparser-package)
      #'bit-vector-p)
(set! (access *length* cl-unparser-package)
      #'length)
(set! (access *aref* cl-unparser-package)
      #'aref)
(set! (access *char* cl-unparser-package)
      #'char)
(set! (access *vectorp* cl-unparser-package)
      #'vectorp)
(set! (access *array-dimensions* cl-unparser-package)
      #'array-dimensions)
(set! (access *array-rank* cl-unparser-package)
      #'array-rank)
(set! (access *stringp* cl-unparser-package)
      #'stringp)

'$split-file

(cl-define is-a-tty? (make-primitive-procedure 'cl-isa-tty-p))
(cl-define internal-write-char (make-primitive-procedure 'cl-internal-write-char))

(defun write
  (object &key
	  ((:stream  *standard-output*)   *standard-output*)
	  ((:escape  *print-escape*)      *print-escape*)
	  ((:radix   *print-radix*)       *print-radix*)
	  ((:base    *print-base*)        *print-base*)
	  ((:circle  *print-circle*)      *print-circle*)
	  ((:pretty  *print-pretty*)      *print-pretty*)
	  ((:level   *print-level*)       *print-level*)
	  ((:length  *print-length*)      *print-length*)
	  ((:case    *print-case*)        *print-case*)
	  ((:array   *print-array*)       *print-array*)
	  ((:gensym  *print-gensym*)      *print-gensym*))
  (cl-unparse-object object)
  (if (is-a-tty? *standard-output*)
      (force-output))
  object)

'$split-file

(defun prin1
 (object &optional stream)
  (let ((*standard-output* (or stream *standard-output*))
	(*print-escape* t))
    (cl-unparse-object object)
    (if (is-a-tty? *standard-output*)
	(force-output))
    object))

'$split-file

(defun print
  (object &optional stream)
  (let ((*standard-output* (or stream *standard-output*))
	(*print-escape* t))
    (internal-write-char #\newline)
    (cl-unparse-object object)
    (internal-write-char #\space)
    (if (is-a-tty? *standard-output*)
	(force-output))
    object))

'$split-file

(defun pprint
  (object &optional stream)
  (let ((*standard-output* (or stream *standard-output*))
	(*print-escape* t)
	(*print-pretty* t))
    (internal-write-char #\newline)
    (cl-unparse-object object)
    (if (is-a-tty? *standard-output*)
	(force-output))
    object))

'$split-file

(defun princ
  (object &optional stream)
  (let ((*standard-output* (or stream *standard-output*))
	(*print-escape* nil))
    (cl-unparse-object object)
    (if (is-a-tty? *standard-output*)
	(force-output))
    object))

(defun terpri (&optional stream)
  (let ((*standard-output* (or stream *standard-output*)))
    (write-char #\newline)
    nil))

;; Charpos will either return a column position or nil,
;; indicating that the column position of the stream is unavaliable.
;; We only perform a freshline if the column isn't zero.

(defun fresh-line (&optional (stream *standard-output*))
  (let ((column (charpos stream)))
    (if (not (= column 0))
	(begin (terpri stream)
	       T)
	nil)))

'$split-file

;; STRINGIFY-OBJECT is an internal printer function produces the printed
;; representation of an object as a string.  It is called by various x-TO-STRING
;; functions below.

(defun stringify-object (object escapep)
  (let ((*standard-output* (make-string-output-stream))
	(*print-escape* escapep))
    (cl-unparse-object object)
    (get-output-stream-string *standard-output*)))

;;; Top-level x-TO-STRING functions.  These functions all take an object
;;; and return that object's printed representation as a string. 

(defun write-to-string 
  (object &key
	  ((:stream  *standard-output*)   *standard-output*)
	  ((:escape  *print-escape*)      *print-escape*)
	  ((:radix   *print-radix*)       *print-radix*)
	  ((:base    *print-base*)        *print-base*)
	  ((:circle  *print-circle*)      *print-circle*)
	  ((:pretty  *print-pretty*)      *print-pretty*)
	  ((:level   *print-level*)       *print-level*)
	  ((:length  *print-length*)      *print-length*)
	  ((:case    *print-case*)        *print-case*)
	  ((:array   *print-array*)       *print-array*)
	  ((:gensym  *print-gensym*)      *print-gensym*))
  (stringify-object object *print-escape*))

'$split-file

(defun prin1-to-string (object)
  (stringify-object object t))

(defun princ-to-string (object)
  (stringify-object object nil))

(cl-define format
  (access cl-format cl-format-package))

;;; Y-OR-N-P prints the message, if any, and reads characters from
;;; *QUERY-IO* until either of "y", or "Y" is seen as an
;;; affirmative, or either "n" or "N" is seen as a negative answer.
;;; It ignores preceding whitespace and asks again if other characters
;;; are seen.

(defun query-readline ()
  (string-trim '(#\space #\tab) (read-line *query-io*)))

(defun y-or-n-p (&optional format-string &rest arguments)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string arguments))
  (do ((ans (query-readline) (query-readline)))
      (())
    (case (unless (zerop (length ans)) (char ans 0))
      ((nil) nil)
      ((#\y #\Y) (return t))
      ((#\n #\N) (return nil))
      (t
       (write-line "Type \"y\" for yes or \"n\" for no. " *query-io*)
       (when format-string
	 (apply #'format *query-io* format-string arguments))))))

;;; YES-OR-NO-P is similar, except that it clears the input buffer,
;;; beeps, and uses READ-LINE to get "YES" or "NO".

(defun yes-or-no-p (&optional format-string &rest arguments)
  (clear-input *query-io*)
  (beep)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string arguments))
  (do ((ans (query-readline) (query-readline)))
      (())
    (cond 
     ((string-equal ans "") nil)
     ((string-equal ans "YES") (return t))
     ((string-equal ans "NO") (return nil))
     (t
      (write-line "Type \"yes\" for yes or \"no\" for no. " *query-io*)
      (when format-string
	(apply #'format *query-io* format-string arguments))))))
