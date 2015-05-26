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

;;; Toned down version of defstruct which is used to 
;;; bootstart the real defstruct system.

;; prefix is the name to use as a prefix for slot names.

(define-macro (mini-defstruct struct prefix)
  (let ((structname (car struct))
	(fields (cdr struct))
	(struct-string (symbol-print-name prefix)))
    (labels ((make-slot-list (start end)
	       (declare (fixnum start end))
	       (if (= start end)
		   (list start)
		   (cons start (make-slot-list (1+ start) end))))

	     (add-prefix (prefix)
               (lambda (s)
		 (make-interned-symbol 
		  (string-append prefix "-" (symbol-print-name s)))))

	     (add-suffix (suffix)
               (lambda (s)
		 (make-interned-symbol 
		  (string-append (symbol-print-name s) suffix)))))

      (let ((slot-list (make-slot-list 1 (length fields)))
	    (predicate-name ((add-suffix "-P") structname))
	    (selector-names (mapcar (add-prefix struct-string) fields))
	    (mutator-names (mapcar (lambda (name) 
				     ((add-prefix (string-append "%SET-" struct-string)) name))
				   fields))
	    (errmsg (string-append "Not of type " (symbol-print-name structname) ": ~A")))
	(labels ((make-selector-definition (field slot-number)
	           `(defun ,field (,structname)
		      (if (,predicate-name ,structname)
			  (vector-ref ,structname ,slot-number)
			  (error ,errmsg ,structname))))
		 (make-selector-mutator (name slot-number)
		   `(defun ,name (,structname value)
		      (if (,predicate-name ,structname)
			  (begin (vector-set! ,structname ,slot-number value)
				 value)
			  (error ,errmsg ,structname)))))
	
	  ;; definitions
	  `(let ((tag (list ',structname)))

	     (defun ,((add-prefix "MAKE") structname) ,fields
	       (vector tag ,@fields))

	     (defun ,predicate-name (,structname)
	       (and (vector? ,structname) 
		    (equal? (vector-ref ,structname 0) tag)))

	     (defun ,((add-prefix "COPY") structname) (,structname)
	       (if (,predicate-name ,structname)
		   (vector-copy ,structname)
		   (error ,errmsg ,structname)))

	     ,@(mapcar #'make-selector-definition selector-names slot-list)
	     ,@(mapcar #'make-selector-mutator mutator-names slot-list)
	     ,@(mapcar (lambda (selector mutator)
			 `(defsetf ,selector ,mutator))
		       selector-names mutator-names)
	     ',structname))))))
