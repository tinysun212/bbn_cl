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
;;;;;;; Patches to standard distribution file INTRPT.SCM
;;;; Should be periodically compared with original to prevent catastrophe

(in-package interrupt-system
  (declare (usual-integrations))

;;; This one is currently identical to the single processor version,
;;; but it may change in the future.

(define ^B-interrupt-handler
  (keep-typeahead
   (lambda (interrupt-character interrupt-enables)
     (with-standard-proceed-point
      (lambda ()
	(breakpoint "^B interrupt" (rep-environment)))))))

(define ^G-interrupt-handler
  (flush-typeahead
   (lambda (interrupt-character interrupt-enables)
     (pause-everything)
     (if (futures-on?)
	 (let ((which-process (top-level-process)))
	   (if (future? which-process)
	       (begin 
		 (within-process which-process
				 (lambda () (abort-to-top-level-driver "Quit!"))))
	       (begin 
		 (abort-to-top-level-driver "^G: Process randomly selected"))))
	 (abort-to-top-level-driver "Quit!")))))

(define ^U-interrupt-handler
  (flush-typeahead
   (lambda (interrupt-character interrupt-enables)
     (if (futures-on?)
	 (let ((which-process (previous-level-process)))
	   (if (future? which-process)
	       (within-process which-process
			       (lambda () (abort-to-previous-driver "Up!")))
	       (abort-to-previous-driver "^U: Process randomly selected")))
	 (abort-to-previous-driver "Up!")))))

(define ^X-interrupt-handler
  (flush-typeahead
   (lambda (interrupt-character interrupt-enables)
     (if (futures-on?)
	 (let ((which-process (current-level-process)))
	   (if (future? which-process)
	       (within-process which-process
	         (lambda () (abort-to-nearest-driver "Abort!")))
	       (abort-to-nearest-driver "^X: Process randomly selected")))
	 (abort-to-nearest-driver "Abort!")))))

(install-keyboard-interrupt! #\G ^G-interrupt-handler)
(install-keyboard-interrupt! #\B ^B-interrupt-handler)
(install-keyboard-interrupt! #\U ^U-interrupt-handler)
(install-keyboard-interrupt! #\X ^X-interrupt-handler)

) ; End of IN-PACKAGE

