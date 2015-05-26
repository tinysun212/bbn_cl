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
(eval-when (compile)
	   (load "clchap7c-macros.bin")
	   (load "clchap7c-comm.bin"))

(proclaim '(insert-touches nil))

(defmacro define-modify-macro (name lambda-list function &optional doc-string)
  "Creates a new read-modify-write macro like PUSH or INCF."
  (let ((other-args nil)
	(rest-arg nil)
	(env (gensym))
	(reference (gensym)))
	     
    ;; Parse out the variable names and rest arg from the lambda list.
    (do ((ll lambda-list (cdr ll))
	 (arg nil))
	((null ll))
      (setq arg (car ll))
      (cond ((eq arg '&optional))
	    ((eq arg '&rest)
	     (if (symbolp (cadr ll))
		 (setq rest-arg (cadr ll))
		 (error "Non-symbol &rest arg in definition of ~S." name))
	     (if (null (cddr ll))
		 (return nil)
		 (error "Illegal stuff after &rest arg in Define-Modify-Macro.")))
	    ((memq arg '(&key &allow-other-keys &aux))
	     (error "~S not allowed in Define-Modify-Macro lambda list." arg))
	    ((symbolp arg)
	     (push arg other-args))
	    ((and (listp arg) (symbolp (car arg)))
	     (push (car arg) other-args))
	    (t (error "Illegal stuff in lambda list of Define-Modify-Macro."))))
    (setq other-args (nreverse other-args))
    `(defmacro ,name (,reference ,@lambda-list &environment ,env)
       ,doc-string
       (multiple-value-bind (dummies vals newval setter getter)
	 (get-setf-method ,reference ,env)
	 (do ((d dummies (cdr d))
	      (v vals (cdr v))
	      (let-list nil (cons (list (car d) (car v)) let-list)))
	     ((null d)
	      (push 
	       (list (car newval)
		     ,(if rest-arg
			  `(list* ',function getter ,@other-args ,rest-arg)
			  `(list ',function getter ,@other-args)))
	       let-list)
	      `(let* ,(nreverse let-list)
		 ,setter)))))))

