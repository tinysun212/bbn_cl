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
;; Chapter 7 -- Control Structure

(proclaim '(insert-touches nil))

(export '(mapc mapcar mapcan mapl maplist mapcon))

;;7.8.4 mapping

(define-macro (cl-mapper-generator name accessor accumulator)
  `(defun ,name (fcn &rest lists)
     (cond
      ((null? lists)
       (error "No arguments to mapping function ~a (~a)" ',name fcn))
      ((null? (cdr lists))
       (labels ((1-loop (list)
		  (if (null? list)
		      '()
		      (let ((x (funcall fcn (,accessor list))))	; force order
			(,accumulator x
				      (1-loop (cdr list)))))))
	 (1-loop (car lists))))
      (else
       (labels ((n-loop (lists)
                  (labels ((scan (lists c)
		            (cond ((null? lists) (((lambda(x)x) c) '() '()))
				  ((null? (car lists)) '())
				  (else
				   (scan (cdr lists)
					 (lambda (args cdrs)
					   (((lambda(x)x) c) (cons (,accessor (car lists)) args)
							     (cons (cdr (car lists)) cdrs))))))))
		    (scan lists
			  (lambda (args cdrs)
			    (let ((x (apply fcn args)))	; force order
			      (,accumulator x
					    (n-loop cdrs))))))))
	 (n-loop lists))))))

(cl-mapper-generator mapc    car            (lambda (x y) nil))
(cl-mapper-generator mapcar  car            cons)
(cl-mapper-generator mapcan  car            nconc)
(cl-mapper-generator mapl    (lambda (x) x) (lambda (x y) nil))
(cl-mapper-generator maplist (lambda (x) x) cons)
(cl-mapper-generator mapcon  (lambda (x) x) nconc)
