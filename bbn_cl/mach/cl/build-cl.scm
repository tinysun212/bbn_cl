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
;;;; Build from Butterfly Scheme to Butterfly Commonlisp

;;; Note that there should be no DEFINEs in this file, as
;;; this file will get evaluated in the load-time environment,
;;; which might not be the global-environment.

;;((make-primitive-procedure '%set-cl-fasload-check!) #t)

;;(sf "build-load.scm")
(load "build-load.bin" '())

;;; Ask the builder for either a compiled or interpreted
;;; system

(newline)
(write-string "Load compiled? (y or n): ")
(set! *load-compiled-commonlisp?* 
      (let ((y-or-n (read))) (eq? y-or-n 'y)))

;;; The build files all want to be compiled deep

(deep-fluid-let!)

;;(tcf "impurify-patch.scm")
(build-load "impurify-patch" '())
;;(tcf "cl-error-boot.scm")
(build-load "cl-error-boot" '())
;;(tcf "symbol.scm")
(build-load "symbol" '())
;;(tcf "pkg-extensions.scm")
(build-load "pkg-extensions" '())
;;(tcf "cl-fixobj.scm")
(build-load "cl-fixobj" '())
;;(tcf "cl-kernel.scm")
(build-load "cl-kernel" '())
;;(tcf "syntax-extensions.scm")
(build-load "syntax-extensions" '())
;;(tcf "errset.scm")
(build-load "errset" '())
;;(tcf "values.scm")
(build-load "values" '())
;;(tcf "make-cl-io.scm")
;;(tcf "cl-parse.scm")
;;(tcf "cl-readtable.scm")
;;(tcf "cl-unparse.scm")
;;(tcf "cl-format.scm")
(build-load "make-cl-io" '())
;;(tcf "cl-pp.scm")
(build-load "cl-pp" '())
;;(tcf "cl-compile-file.scm")
(build-load "cl-compile-file" '())
;;(tcf "boot-np-inline.scm")
(build-load "boot-np-inline" '())
;;(tcf "exported-scheme-builtins.scm")
(build-load "exported-scheme-builtins" '())

;;; Commonlisp is turned on here

(cl-mode #t -cl-io #f)
(set! (access *optimize-imperative-constructs?* syntaxer-package) #t)
(set! *commonlisp-messages* #t)

;;(cl-cf "cl-macros.scm")
(build-load "cl-macros" '())
;;(cl-cf "integration-stubs.scm")
(build-load "integration-stubs" '())

;;; Turn on Commonlisp declaration processing

(set! default-declarations '())
(set! *process-declarations* #t)

;;(cl-cf "optimizer-interface.scm")
(build-load "optimizer-interface" '())
;;(cl-cf "np-inline.scm")
(build-load "np-inline" '())
;;(cl-cf "initial-optimizers.scm")
(build-load "initial-optimizers" '())
;;(cl-cf "proclaim.scm")
(build-load "proclaim" '())
;;(cl-cf "arith.scm")
(build-load "arith" '())
;;(cl-cf "cl-array-prims.scm")
(build-load "cl-array-prims" '())
;;(cl-cf "localinit.scm")
(build-load "localinit" '())
;;(cl-cf "cl-util.scm")
(build-load "cl-util" '())
;;(cl-cf "cl-util2.scm")
(build-load "cl-util2" '())
;;(cl-cf "clchap5-a.scm")
(build-load "clchap5-a" '())
;;(cl-cf "clchap5-b.scm")
(build-load "clchap5-b" '())
;;(cl-cf "defun.scm")
(build-load "defun" '())

(proclaim '(special *package*))

;;(cl-cf "predicates.scm")
(build-load "predicates" '())
;;(cl-cf "clchap7.scm")
(build-load "clchap7" '())
;;(cl-cf "clchap7b.scm")
(build-load "clchap7b" '())
;;(cl-cf "clchap9.scm")
(build-load "clchap9" '())
;;(cl-cf "clchap10.scm")
(build-load "clchap10" '())
;;(cl-cf "clchap5a.scm")
(build-load "clchap5a" '())
;;(cl-cf "arith-constants.scm")
(build-load "arith-constants" '())
;;(cl-cf "lambda-constants.scm")
(build-load "lambda-constants" '())
;;(cl-cf "equalp.scm")
(build-load "equalp" '())
;;(cl-cf "clchap13.scm")
(build-load "clchap13" '())
;;(cl-cf "clchap15.scm")
(build-load "clchap15" '())
;;(cl-cf "clchap7a.scm")
(build-load "clchap7a" '())
;;(cl-cf "clchap8-macros.scm")
;;(cl-cf "clchap8-comm.scm")
;;(cl-cf "clchap8-a.scm")
;;(cl-cf "clchap8-b-1.scm")
;;(cl-cf "clchap8-b-2.scm")
;;(cl-cf "clchap8-c.scm")
(build-load "clchap8-comm" '())
(build-load "clchap8-a" '())
(build-load "clchap8-b-1" '())
(build-load "clchap8-b-2" '())
(build-load "clchap8-c" '())
;;(cl-cf "clchap8a.scm")
(build-load "clchap8a" '())
;;(cl-cf "clchap7c-macros.scm")
;;(cl-cf "clchap7c-comm.scm")
;;(cl-cf "clchap7c-a.scm")
;;(cl-cf "clchap7c-b-1.scm")
;;(cl-cf "clchap7c-b-2.scm")
(build-load "clchap7c-comm" '())
(build-load "clchap7c-a" '())
(build-load "clchap7c-b-1" '())
(build-load "clchap7c-b-2" '())
;;(cl-cf "check-type.scm")
(build-load "check-type" '())
;;(cl-cf "clchap7d-a.scm")
;;(cl-cf "clchap7d-b.scm")
(build-load "clchap7d-a" '())
(build-load "clchap7d-b" '())
;;(cl-cf "clchap15a-macros.scm")
;;(cl-cf "clchap15a.scm")
(build-load "clchap15a" '())
;;(cl-cf "clchap25.scm")
(build-load "clchap25" '())
;;(cl-cf "clchap7e.scm")
(build-load "clchap7e" '())
;;(cl-cf "clchap7f.scm")
(build-load "clchap7f" '())
;;(cl-cf "typespec.scm")
(build-load "typespec" '())
;;(cl-cf "typep.scm")
(build-load "typep" '())
;;(cl-cf "cl-array.scm")
(build-load "cl-array" '())
;;(cl-cf "cl-coerce.scm")
(build-load "cl-coerce" '())
;;(cl-cf "subtypep.scm")
(build-load "subtypep" '())
;;(cl-cf "sequence-simple.scm")
;;(cl-cf "sequence-map.scm")
;;(cl-cf "sequence-modify.scm")
;;(cl-cf "sequence-search.scm")
;;(cl-cf "sequence-sort.scm")
(build-load "sequence-simple" '())
(build-load "sequence-map" '())
(build-load "sequence-modify" '())
(build-load "sequence-search" '())
(build-load "sequence-sort" '())
;;(cl-cf "normalize-type-expr.scm")
(build-load "normalize-type-expr" '())
;;(cl-cf "clchap20.scm")
(build-load "clchap20" '())
;;(cl-cf "cl-array-2.scm")
(build-load "cl-array-2" '())
;;(cl-cf "cl-string.scm")
(build-load "cl-string" '())
;;(cl-cf "clchap11.scm")
(build-load "clchap11" '())
;;(cl-cf "mini-defstruct.scm")
;;(cl-cf "clchap19.scm")
(build-load "clchap19" '())
;;(cl-cf "clchap16-macros.scm")
;;(cl-cf "clchap16-comm.scm")
;;(cl-cf "clchap16-a-1.scm")
;;(cl-cf "clchap16-a-2.scm")
;;(cl-cf "clchap16-b.scm")
(build-load "clchap16-comm" '())
(build-load "clchap16-a-1" '())
(build-load "clchap16-a-2" '())
(build-load "clchap16-b" '())
;;(cl-cf "arith-defs-i.scm")
;;(cl-cf "arith-defs-j.scm")
;;(cl-cf "arith-defs-k.scm")
(build-load "arith-defs-i" '())
(build-load "arith-defs-j" '())
(build-load "arith-defs-k" '())
;;(cl-cf "filesys.scm")
(build-load "filesys" '())
;;(cl-cf "stream-interface.scm")
(build-load "stream-interface" '())
;;(cl-cf "reader-interface.scm")
(build-load "reader-interface" '())
;;(cl-cf "printer-interface.scm")
(build-load "printer-interface" '())
;;(cl-cf "error.scm")
(build-load "error" '())
;;(cl-cf "cl-errset.scm")
(build-load "cl-errset" '())
;;(cl-cf "modules.scm")
(build-load "modules" '())
;;(cl-cf "parallel.scm")
(build-load "parallel" '())
;;(cl-cf "misc-exports.scm")
(build-load "misc-exports" '())
;;(cl-cf "package-common.scm")
(build-load "package-common" '())
;;(cl-cf "package-a.scm")
(build-load "package-a" '())
;;(cl-cf "package-b.scm")
(build-load "package-b" '())
;;(cl-cf "package-c.scm")
(build-load "package-c" '())
;;(cl-cf "package-d.scm")
(build-load "package-d" '())
;;(cl-cf "package-e.scm")
(build-load "package-e" '())
;;(cl-cf "system-config.scm")
(build-load "system-config" '())
;;(cl-cf "system-init.scm")
(build-load "system-init" '())
;;(cl-cf "cl-fluids.scm")
(build-load "cl-fluids" '())
;;(cl-cf  "setup-packages.scm")
(build-load "setup-packages" '())

;;; Remove the macro def of proclaim.

(remove-syntax! 'proclaim)

;;; Setup *features*

(push ':butterfly *features*)
(push ':ieee-floating-point *features*)

;;; Setup the *lisp* and *system* packages

(setup-packages)

(quasiquote->defmacro)

;;; Redefine load to be commonlisp's load

(setf (symbol-function 'load) #'cl-load)

;;; Write information about this build

(write-build-info)

;;; Purify the code from the build, if not on butterfly

(if (not is-a-butterfly?)
    (begin
      (newline)
      (newline)
      (princ ";;; Purifying the build code...")
      (purify *build-code* t)
      (newline)))

;;; Get into the proper environment

(%ge *commonlisp-user-environment*)

;;; Some filename default hackery

(set! *load-source-filename-extension* "lisp")
(set! (access sf/default-input-pathname (access package/top-level package/scode-optimizer))
      ((access make-pathname '()) false false false false "lisp" 'newest))

;;; Turn on caches

(set! *use-subtract-futures-cache* t)
(set! *use-subtype-cache* t)

;;; Turn on saving of source code

(set! *save-commonlisp-source* t)

;;; Turn off the debug print of "Expanding..."

(set! *print-defs?* #f)

;;; Turn off forced eval of defmacro, defconstant, etc., at syntax time

(set! *build-time-eval* #f)

;;; Update version

(set! *lisp-implementation-version* "2.00")

;;; Get into the proper package

(in-package 'user)

;;; Have the builder finish the build

(null (princ (format nil
"
;;;
;;; Be sure to do the following things by hand:
;;;

(cl-mode -cl-io #t) (proclaim '(insert-touches t)) (proclaim '(lr-evaluation t)) (disk-save \"~a\" \"Butterfly Common Lisp\")

"

(if *load-compiled-commonlisp?*
    "cl-c.band"
    "icl-c.band")

)))




