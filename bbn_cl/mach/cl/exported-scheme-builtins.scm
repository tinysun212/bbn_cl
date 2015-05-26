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
;; Scheme functions required by the Butterfly Common Lisp implementation
;; These functions will be referenced by mutated names, hence the need
;; for this module

(declare (usual-integrations))

(define (export-scheme-builtins name-list)
  (let ((prim-count 0)
	(proc-count 0))

    (define (export-scheme-builtin name)
      (if (not (lexical-unreferenceable? '() name))
	  (let ((proc (lexical-reference '() name)))
	    (if (or (procedure? proc)
		    (primitive-procedure? proc))
		(let ((cl-name (fundefsym name)))
		  (local-assignment '() cl-name proc)
		  (if (primitive-procedure? proc)
		      (set! prim-count (1+ prim-count))
		      (set! proc-count (1+ proc-count))))))))

    (newline)
    (display ";;; Exporting Scheme procedures to Common Lisp...")
		
    (mapc export-scheme-builtin name-list)

    (newline)
    (display ";;; ")
    (display (length name-list))
    (display " symbols were examined.")
    (newline)
    (display ";;; ")
    (display (+ prim-count proc-count))
    (display " procedures were exported (")
    (display prim-count)
    (display " primitives and ")
    (display proc-count)
    (display " compound procedures)")))

(define (get-all-symbols)
  (let ((symbols '()))
    (symbol-table-iterator 
     (lambda (x) 
       (if (not (or (eq? x #f)
		    (eq? x #t)))
	   (set! symbols (cons x symbols))))
     (vector-ref (get-fixed-objects-vector)
		 (fixed-objects-vector-slot 'obarray)))
    symbols))

(export-scheme-builtins (get-all-symbols))
