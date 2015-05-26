;;; -*-Scheme-*-
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

;;; Crock to define a bunch of special object types.
;;; Done here because it must happen late in the cold load build.
;;; Needs to be implemented reasonably sometime.

(declare (usual-integrations))

((access add-unparser-special-object! unparser-package)
 (access type-object-tag type-system)
 (lambda (type-object)
   (unparse-with-brackets
    (lambda ()
      (write-string "TYPE-OBJECT ")
      (write (or (type-object-name type-object)
		 (hash type-object)))))))

((access add-unparser-special-object! unparser-package)
 (vector-ref (get-fixed-objects-vector)
	     (fixed-objects-vector-slot 'STATE-SPACE-TAG))
 (lambda (state-space)
   (unparse-with-brackets
    (lambda ()
      (write-string "STATE-SPACE ")
      (write (hash state-space))))))

((access add-unparser-special-object! unparser-package)
 (vector-ref (get-fixed-objects-vector)
	     (fixed-objects-vector-slot 'STATE-POINT-TAG))
 (lambda (state-point)
   (unparse-with-brackets
    (lambda ()
      (write-string "STATE-POINT ")
      (write (hash state-point))))))

((access add-unparser-special-pair! unparser-package)
 (access syntax-table-tag syntaxer-package)
 (lambda (syntax-table)
   (unparse-with-brackets
    (lambda ()
      (write-string "SYNTAX-TABLE ")
      (write (hash syntax-table))))))

((access add-unparser-special-object! unparser-package)
 (vector-ref (make-pathname false false false false false false) 0)
 (lambda (pathname)
   (unparse-with-brackets
    (lambda ()
      (write-string "PATHNAME ")
      (write (pathname->string pathname))))))
