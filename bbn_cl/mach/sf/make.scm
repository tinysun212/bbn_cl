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

;;;; SCode Optimizer: System Construction

(in-package system-global-environment
(declare (usual-integrations))

(define sf)
(define sfu? false)
(define sf/set-default-syntax-table!)
(define sf/set-file-syntax-table!)
(define sf/add-file-declarations!)

(define package/scode-optimizer
  (make-environment
    (define package/top-level	(make-environment))
    (define package/transform	(make-environment))
    (define package/integrate	(make-environment))
    (define package/cgen	(make-environment))
    (define package/expansion	(make-environment))
    (define package/declarations (make-environment))
    (define package/copy	(make-environment))
    (define package/free	(make-environment))
    (define package/change-type	(make-environment))))

(in-package package/scode-optimizer

  (define scode-optimizer/system
    (make-environment
      (define :name "SF")
      (define :version 3)
      (define :modification 15)
      (define :files)

      (define :files-lists
	(list
	 (cons package/scode-optimizer
	       '("mvalue.com"		;Multiple Value Support
		 "eqsets.com"		;Set Data Abstraction
		 "pthmap.com"		;Pathname Map Abstraction
		 "object.com"		;Data Structures
		 "emodel.com"		;Environment Model
		 "gconst.com"		;Global Primitives List
		 "usicon.com"		;Usual Integrations: Constants
		 "tables.com"		;Table Abstractions
		 "packag.com"		;Global packaging
		 ))
	 (cons package/top-level
	       '("toplev.com"))		;Top Level
	 (cons package/transform
	       '("xform.com"))		;SCode -> Internal
	 (cons package/integrate
	       '("subst.com"))		;Beta Substitution Optimizer
	 (cons package/cgen
	       '("cgen.com"))		;Internal -> SCode
	 (cons package/expansion
	       '("usiexp.com"))		;Usual Integrations: Expanders
	 (cons package/declarations
	       '("pardec.com"))		;Declaration Parser
	 (cons package/copy
	       '("copy.com"))		;Copy Expressions
	 (cons package/free
	       '("free.com"))		;Free Variable Analysis
	 (cons package/change-type
	       '("chtype.com"))		;Type interning
	 ))))

  (load-system! scode-optimizer/system)

  (scode-optimizer/initialize!))

) ;;; end IN-PACKAGE SYSTEM-GLOBAL-ENVIRONMENT
