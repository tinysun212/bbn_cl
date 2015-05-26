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
;;;(declare (usual-integrations))

(define (tak x y z)
  (let recurse ((x x) (y y) (z z))
       (if (not (< y x))
	   z
	 (recurse (recurse (-1+ x) y z)
		  (recurse (-1+ y) z x)
		  (recurse (-1+ z) x y)))))

(define (fib-o-fib n)
  (if (< n 2)
      1
      (+ (future (fib-o-fib (-1+ n)))
	 (future (fib-o-fib (- n 2)))
	 1)))
