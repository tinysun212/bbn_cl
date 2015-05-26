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
;;;#| -*-Scheme-*-

Copyright (c) 1988 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; SCode Optimizer: Global Package Structure

(declare (usual-integrations))

(define read-externs-file)
(define declarations/known?)
(define declarations/make-null)
(define declarations/parse)
(define declarations/binders)
(define declarations/original)
(define declarations/map)
(define declarations/for-each-variable)
(define declarations/integrated-variables)
(define operations->external)
(define transform/expression)
(define transform/recursive)
(define integrate/expression)
(define integrate/get-top-level-block)
(define variable/final-value)
(define copy/expression)
(define copy/expression/extern)
(define free/expression)
(define cgen/expression)
(define intern-type)
(define usual-integrations/expansion-names)
(define usual-integrations/expansion-values)
(define usual-integrations/expansion-alist)
(define integrate/procedure)
(define integrate/file)
(define integrate/sexp)
(define integrate/scode)

(define (scode-optimizer/initialize!)
  (set! (access sf system-global-environment)
	(access sf package/top-level))
  (set! (access sf/set-default-syntax-table! system-global-environment)
	(access sf/set-default-syntax-table! package/top-level))
  (set! (access sf/set-file-syntax-table! system-global-environment)
	(access sf/set-file-syntax-table! package/top-level))
  (set! (access sf/add-file-declarations! system-global-environment)
	(access sf/add-file-declarations! package/top-level))
  (set! read-externs-file
	(access read-externs-file package/top-level))
  (set! declarations/known?
	(access declarations/known? package/declarations))
  (set! declarations/make-null
	(access declarations/make-null package/declarations))
  (set! declarations/parse
	(access declarations/parse package/declarations))
  (set! declarations/binders
	(access declarations/binders package/declarations))
  (set! declarations/original
	(access declarations/original package/declarations))
  (set! declarations/map
	(access declarations/map package/declarations))
  (set! declarations/for-each-variable
	(access declarations/for-each-variable package/declarations))
  (set! declarations/integrated-variables
	(access declarations/integrated-variables package/declarations))
  (set! operations->external
	(access operations->external package/declarations))
  (set! transform/expression
	(access transform/top-level package/transform))
  (set! transform/recursive
	(access transform/recursive package/transform))
  (set! integrate/expression
	(access integrate/top-level package/integrate))
  (set! integrate/get-top-level-block
	(access integrate/get-top-level-block package/integrate))
  (set! variable/final-value
	(access variable/final-value package/integrate))
  (set! copy/expression
	(access copy/external/intern package/copy))
  (set! copy/expression/extern
	(access copy/external/extern package/copy))
  (set! free/expression
	(access free/expression package/free))
  (set! cgen/expression
	(access cgen/external package/cgen))
  (set! intern-type
	(access change-type/external package/change-type))
  (set! usual-integrations/expansion-names
	(access usual-integrations/expansion-names package/expansion))
  (set! usual-integrations/expansion-values
	(access usual-integrations/expansion-values package/expansion))
  (set! usual-integrations/expansion-alist
	(access usual-integrations/expansion-alist package/expansion))
  (set! integrate/procedure
	(access integrate/procedure package/top-level))
  (set! integrate/file
	(access integrate/file package/top-level))
  (set! integrate/sexp
	(access integrate/sexp package/top-level))
  (set! integrate/scode
	(access integrate/scode package/top-level))
  (usual-integrations/cache!))

;;; Grumble... there is no natural place to put this.

(define (warn message . objects)
  (newline)
  (write-string "Warning! ")
  (write-string message)
  (for-each (lambda (object)
	      (write-string " ")
	      (write object))
	    objects))

(define scode-assignment?
  (access assignment? system-global-environment))

(define scode-open-block?
  (access open-block? system-global-environment))

(define scode-sequence?
  (access sequence? system-global-environment))

(define scode-the-environment?
  (access the-environment? system-global-environment))



