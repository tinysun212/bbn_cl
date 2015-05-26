;;; -*-Scheme-*-
;;;
<<<<<<< sbuild.scm
;;;	$Header: sbuild.scm,v 13.90 88/06/06 09:02:47 ajc Exp $
=======
;;;	$Header: sbuild.scm,v 13.90 88/06/06 09:02:47 ajc Exp $
>>>>>>> 13.80.2.2
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; 6.001 Student Environment

(in-package system-global-environment
(declare (usual-integrations))

(define student-system
  (make-environment

(define :name "Student")
(define :version 13)
(define :modification 4)

(define :files
  '(
    "compat.bin"
    ;; "file.bin"
    "Sgraph.bin"
    ;; "login.bin"
    "stream.bin"
    "strmac.bin"
    "xplode.bin"
    "genenv.bin"
    "studen.bin"
    ;; "stunit.bin"
    ))

(define (:load)
  (let ((exps (map (lambda (filename) (fasload filename)) :files)))
    (newline)
    (write-string "Purify")
    (purify exps true)
    (for-each (lambda (exp filename)
		(newline)
		(write-string "Eval ")
		(write filename)
		(scode-eval exp system-global-environment))
	      exps
	      :files))
  (add-system! student-system))

;;; end STUDENT-SYSTEM package.
))

((access :load student-system))
*the-non-printing-object*)
