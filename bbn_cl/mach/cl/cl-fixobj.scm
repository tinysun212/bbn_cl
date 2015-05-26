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
;;; Creation of and definitions for the CL fixed objects vector.
;;;  Most future fixed objects should be addable here, without
;;;  the need to update the main fixed objects vector. The only
;;;  requirement is that such objects only be used by the ucode
;;;  after their definition here.
;;; This file must be syntaxed and loaded after pkg-extensions so that 
;;;  the keywords get placed correctly in the keyword package.

(define *cl-fixed-objects-size* 41)

(define *cl-fixed-objects* (make-vector *cl-fixed-objects-size*))

(define (%get-cl-fixed-obj n)
  (vector-ref *cl-fixed-objects* n))

(define (%set-cl-fixed-obj n s)
  (vector-set! *cl-fixed-objects* n s))

(vector-set! (get-fixed-objects-vector)
	     (fixed-objects-vector-slot 'cl-fixed-objects)
	     *cl-fixed-objects*)

(%set-cl-fixed-obj 0 (keyword-intern "INPUT"))
(%set-cl-fixed-obj 1 (keyword-intern "OUTPUT"))
(%set-cl-fixed-obj 2 (keyword-intern "IO"))
(%set-cl-fixed-obj 3 (keyword-intern "ERROR"))
(%set-cl-fixed-obj 4 (keyword-intern "CREATE"))
(%set-cl-fixed-obj 5 (keyword-intern "NEW-VERSION"))
(%set-cl-fixed-obj 6 (keyword-intern "RENAME"))
(%set-cl-fixed-obj 7 (keyword-intern "RENAME-AND-DELETE"))
(%set-cl-fixed-obj 8 (keyword-intern "OVERWRITE"))
(%set-cl-fixed-obj 9 (keyword-intern "APPEND"))
(%set-cl-fixed-obj 10 (keyword-intern "SUPERSEDE"))
(%set-cl-fixed-obj 11 (keyword-intern "PROBE"))
(%set-cl-fixed-obj 12 (keyword-intern "EOF"))
(%set-cl-fixed-obj 13 '*standard-input*)
(%set-cl-fixed-obj 14 '*standard-output*)
(%set-cl-fixed-obj 15 '*terminal-io*)
(%set-cl-fixed-obj 16 't)
(%set-cl-fixed-obj 17 'string-char)
(%set-cl-fixed-obj 18 (keyword-intern "START"))
(%set-cl-fixed-obj 19 (keyword-intern "END"))
(%set-cl-fixed-obj 20 (keyword-intern "ABORT"))
(%set-cl-fixed-obj 21 (keyword-intern "OK"))
(%set-cl-fixed-obj 22 (char-set #\newline))
(%set-cl-fixed-obj 23 'bit)
(%set-cl-fixed-obj 24 'mod)
(%set-cl-fixed-obj 25 '(unsigned-byte 1))
(%set-cl-fixed-obj 26 '(unsigned-byte 2))
(%set-cl-fixed-obj 27 '(unsigned-byte 4))
(%set-cl-fixed-obj 28 '(unsigned-byte 8))
(%set-cl-fixed-obj 29 '(unsigned-byte 16))

;; Object 30 is added in cl-error.scm

(%set-cl-fixed-obj 31 'signed-byte)
(%set-cl-fixed-obj 32 'unsigned-byte)
(%set-cl-fixed-obj 33 '(signed-byte 1))
(%set-cl-fixed-obj 34 '(signed-byte 2))
(%set-cl-fixed-obj 35 '(signed-byte 4))
(%set-cl-fixed-obj 36 '(signed-byte 8))
(%set-cl-fixed-obj 37 '(signed-byte 16))

(%set-cl-fixed-obj 38 (keyword-intern "INTERNAL"))
(%set-cl-fixed-obj 39 (keyword-intern "EXTERNAL"))
(%set-cl-fixed-obj 40 (keyword-intern "INHERITED"))
