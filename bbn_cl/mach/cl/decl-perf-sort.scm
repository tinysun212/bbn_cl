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
;;;(proclaim '(insert-touches nil))

(defun decl-find-all (key l)
  (list-transform-positive l (lambda (el) (eq? (car el) (car key)))))

(defun decl-remove-all (key l)
  (list-transform-negative l (lambda (el) (eq? (car el) (car key)))))

(defun sort-decl-perf-list (l)
  (if (null? l)
      '()
      (cons (decl-find-all (car l) l)
	    (sort-decl-perf-list (decl-remove-all (car l) l)))))

(defun end-time? (l)
  (eq? (second l) 'end))

(defun start-time? (l)
  (eq? (second l) 'start))

(defun get-end-time (segment)
  (if (end-time?  segment)
      (third segment)
      (error "Not an end time segment:~A" segment)))

(defun get-start-time (segment)
  (if (start-time?  segment)
      (third segment)
      (error "Not a start time segment:~A" segment)))

(defun analyze-decl-perf-element (l)
  (labels ((analyze-time-segment (l)
	     (multiple-value-bind (end-time-segment number-of-calls rest)
	       (get-end-time-segment (cdr l))
	       (values (- (get-end-time end-time-segment)
			  (get-start-time (car l)))
		       (1+ number-of-calls)
		       rest)))
	   (get-end-time-segment (l)
	      (do ((l l)
		   (count 0))
		  ((end-time? (car l))
		   (values (car l) count (cdr l)))
		(if (null? l)
		    (error "No matching end-segment")
		    (multiple-value-bind (inner-time inner-n rest)
		      (analyze-time-segment l)
		      (setq l rest
			    count (+ count inner-n)))))))
    (let ((name (caar l)))
      (multiple-value-bind (time count rest)
	(analyze-time-segment l)
	(if (null? rest)
	    (list name time count rest)
	    (let ((more (analyze-decl-perf-element rest)))
	      (list name
		    (+ time (second more))
		    (+ count (third more))
		    (fourth more))))))))

(defun analyze-decl ()
  (let ((x (sort-decl-perf-list *decl-perf-list*)))
    (let ((y (mapcar (lambda (el) (analyze-decl-perf-element (reverse el))) x)))
      (let ((z (sort y (lambda (a b) (> (second a) (second b))))))
	(format t "~2%~30@<~A~>~10<~A~>~10<~A~>~%~50,,,'-<~>~2%" "Function" "Time" "N calls")
	(dolist (l z 'done)
	  (format t "~30@<~A~>~10<~A~>~10<~A~>~%"
		  (first l) (second l) (third l)))))))

(defun syn (x)
  (syntax x *rep-current-syntax-table*))

(defmacro decl-time (x)
  `(progn (*clear-decl-perf*)
	  (setq *decl-perf* t)
	  (format t "~2%Evaluating ... ")
	  ,x
	  (format t "~2%Analyzing  ... ")
	  (setq *decl-perf* nil)
	  (analyze-decl)))
