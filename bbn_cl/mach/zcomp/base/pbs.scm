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
;;;(in-package system-global-environment

(declare (usual-integrations))

(vector-set! (access dispatch-vector unparser-package)
	     (microcode-type 'VECTOR-1B)
	     (lambda (bit-string)
	       (define (loop n)
		 (if (not (negative? n))
		     (begin (write-char (if (bit-string-ref bit-string n)
					    #\1 #\0))
			    (loop (-1+ n)))))
	       (write-string "#*")
	       (loop (-1+ (bit-string-length bit-string)))))

(in-package parser-package
   (define-char-special #\*
     (lambda ()
       (discard-char)
       (let ((string (read-atom)))
	 (unsigned-integer->bit-string
	  (string-length string)
	  (fluid-let ((*parser-radix* 2))
	    (or (parse-number string)
		(error "READ: Bad syntax for bit-string"))))))))

)
