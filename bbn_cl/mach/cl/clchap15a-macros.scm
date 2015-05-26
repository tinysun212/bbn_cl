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

(defmacro satisfies-the-test (item elt)
  `(cond (testp
	  (funcall test ,item (funcall key ,elt)))
	 (notp
	  (not (funcall test-not ,item (funcall key ,elt))))
	 (else
	  (funcall test ,item (funcall key ,elt)))))

;;  "Substitutes new for subtrees matching old."

(defmacro nsublis-macro ()
	  '(if notp
	       (assoc (funcall key subtree) alist :test-not test-not)
	       (assoc (funcall key subtree) alist :test test)))

(defmacro with-set-keys (funcall)
  `(cond ((and testp notp) (error "Test and test-not both supplied."))
	 (notp ,(append funcall '(:key key :test-not test-not)))
	 (t ,(append funcall '(:key key :test test)))))


;;; Destination and source are setf-able and many-evaluable.
;;; Sets the source to the cdr, and "conses" the 1st elt of source to destination.

(defmacro steve-splice (source destination)
  `(let ((temp ,source))
     (setf ,source (Cdr ,source)
	   (cdr temp) ,destination
	   ,destination temp)))

(defmacro assoc-guts (test-guy)
  `(labels ((search-loop (alist)
	      (cond ((endp alist)
		     nil)
		    (,test-guy
		     (car alist))
		    (else
		     (search-loop (cdr alist))))))
     (search-loop alist)))
