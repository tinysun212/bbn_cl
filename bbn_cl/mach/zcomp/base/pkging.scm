#| -*-Scheme-*-

$Header: pkging.scm,v 1.3 88/08/31 10:36:00 jinx Exp $
$MIT-Header: pkging.scm,v 4.2 87/12/30 06:59:11 GMT cph Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; Compiler Package Structure

(declare (usual-integrations))


(define touchifier-package
  (make-environment))

(define compiler-package
  (make-environment

    ;; Subpackages
    (define decls-package
      (make-environment))
    (define fg-generator-package
      (make-environment))
    (define fg-optimizer-package
      (make-environment))
    (define rtl-generator-package
      (make-environment))
    (define rtl-cse-package
      (make-environment))
    (define rtl-optimizer-package
      (make-environment))
    (define debugging-information-package
      (make-environment))
    (define lap-syntax-package
      (make-environment))
    (define bit-package
      (make-environment))
    (define disassembler-package
      (make-environment))

    ;; Exports from subpackages
    (define lap:syntax-instruction)
    (define instruction-append)

    ))
