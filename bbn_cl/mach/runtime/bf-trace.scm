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
(declare (usual-integrations))

(set! (access trace-display advice-package)
      (named-lambda (trace-display proc args #!optional result)

	(let ((output-port (or (access trace-output-port advice-package)
			       rep-output-window (rep-output-port)))
	      (width 160))
	  
	  (let ((string
		 (with-output-to-truncated-string
		  width
		  (lambda ()
		    (write-string "\n[")
		    (if (futures-on?)
			(let ((this-future (current-future)))
			  (if (not (eq? rep-future-id 
					(future-ref this-future
						    future-metering-slot)))
			      (begin
				(display (future-ref this-future
						     future-spawn-tree-slot))
				(write-string " ")))))
		    (if (unassigned? result)
			(write-string "Entering ")
			(begin
			  (if (> (length result) 1)
			      (begin (write-string "Multiple values:")
				     (for-each (lambda (x) (write-string " ") (write x)) result))
			      (write (car result)))
			  (write-string " <=== ")))
		    (write proc)
		    (for-each
		     (lambda (arg)
		       (write-char #\Space)
		       (write arg))
		     args)
		    (write-string "]")))))
	    (if (car string)
		(write-string
		 (string-append (substring (cdr string) 0 (- width 5))
				" ... ")
		 output-port)
		(write-string (cdr string) output-port)))

	  (if (unassigned? result)
	      *the-non-printing-object*
	      result))))
