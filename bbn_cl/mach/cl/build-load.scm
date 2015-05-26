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
(define *common-lisp-filenames* '())

(define *load-compiled-commonlisp?* #f)

(define *build-code* (vector-cons 512 '()))
(define *build-index* 0)

;;;
;;; filename is just the root.
;;;

(define prim-file-write-date (make-primitive-procedure 'cl-file-write-date))

(define (build-load filename environment)
  (set! *common-lisp-filenames*
	(cons filename *common-lisp-filenames*))
  (let ((bin (string-append filename ".bin"))
	(com (string-append filename ".com")))
    (define (inform file)
      (newline)
      (princ ";;; Loading ")
      (prin1 file)
      file)
    (call-with-fasl-input-file
	(if *load-compiled-commonlisp?*
	    (if (> (prim-file-write-date bin)
		   (prim-file-write-date com))
		(begin
		  (newline)
		  (princ ";;; Build Warning: ")
		  (prin1 bin)
		  (princ " is more recent than ")
		  (prin1 com)
		  (inform bin))
		(inform com))
	    (inform bin))
      (lambda (fd)
	(let load-loop ((scode (fasload-fd fd *package*)))
	  (if (eq? scode eof-error-code)
	      'DONE
	      (begin
		(vector-set! *build-code* *build-index* scode)
		(set! *build-index* (1+ *build-index*))
		(scode-eval scode environment)
		(load-loop (fasload-fd fd *package*)))))))))
	    
(define (write-build-info)
  (let ((f (open-output-file "build-info.scm")))
    (write `("Date:" ,(date) "Time:" ,(time)) f)
    (pp *common-lisp-filenames* f)
    (close-output-port f))
  'BUILD-INFORMATION-WRITTEN)

(define (cl-cf input . options)
  (if *load-compiled-commonlisp?*
      (apply (access cf compiler-package) `(,input ,@options ,@'(touch #f)))
      (apply sf `(,input ,@options))))

(define (tcf input . options)
  (if *load-compiled-commonlisp?*
      (apply (access cf compiler-package) `(,input ,@options ,@'(touch #t)))
      (apply sf `(,input ,@options))))

