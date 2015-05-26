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
;;; System configuration file for system

(proclaim '(insert-touches nil))

;;; ***************************************************** ;;;
;;;                                                       ;;;
;;;    Hard parameters, should change only per release    ;;;
;;;                                                       ;;;
;;; ***************************************************** ;;;

;; Generic name of this particular Common Lisp implementation

(defun lisp-implementation-type ()
  "Butterfly Common Lisp")

;; Version of this particular Common Lisp implementation

(defun lisp-implementation-version ()
  *lisp-implementation-version*)

;; Generic name of the computer hardware on which Common Lisp
;; is running

(defun machine-type ()
  "GP1000")

;; Version of the computer hardware on which Common Lisp is
;; running.  [Can we get this from the operating system?]
;; [Rich Shaaf says no.]

(defun machine-version ()
  nil)

;; The particular instance of the computer hardware on which
;; Common Lisp is running

(defun machine-instance ()
  ((make-primitive-procedure 'cl-get-host-name)))

;; Generic name of any relevant suppporting software

(defun software-type ()
  "GP1000 Mach")

;; Version of of the relevant supporting software
;; [Can we get this from the operation system?]
;; [Rich Shaaf says no.]

(defun software-version ()
  "2.00")

;;; ***************************************************** ;;;
;;;                                                       ;;;
;;; These variables are set up by the user, who must then ;;;
;;;    disk-save a new band                               ;;;
;;;                                                       ;;;
;;; ***************************************************** ;;;

(defvar *short-site-name*)
(defvar *long-site-name*)

(setq *short-site-name* '())
(setq *long-site-name* '())

(defun short-site-name (&key (new-name nil new-name-supplied-p))
  (if new-name-supplied-p
      (progn
	(check-type new-name string)
	(setq *short-site-name* new-name))
      (if (null *short-site-name*)
	  (progn 
	    (warn "System configuration has not been performed~%~
                   (see manual entry for function SHORT-SITE-NAME).")
	    "Unknown site")
	  *short-site-name*)))

(defun long-site-name (&key (new-name nil new-name-supplied-p))
  (if new-name-supplied-p
      (progn
	(check-type new-name string)
	(setq *long-site-name* new-name))
      (if (null *long-site-name*)
	  (progn 
	    (warn "System configuration has not been performed~%~
                   (see manual entry for function LONG-SITE-NAME).")
	    "Unknown site")
	  *long-site-name*)))

