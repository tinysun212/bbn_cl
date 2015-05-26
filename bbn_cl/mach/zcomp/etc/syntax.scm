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
;;;;;; -*-Scheme-*- Build a butterfly compiler from scratch

(define scratch/root-directory
  "/usr/blisp/mach/zcomp")

(define scratch/subdirectories
  '("base" "fggen" "fgopt" "rtlbase" "rtlgen" "rtlopt" "back"
	   "machines/butterfly"))

(set-working-directory-pathname! scratch/root-directory)
(load '("etc/direct" "etc/butils") system-global-environment)

(define (scratch/sf-and-load files environment)
  (sf-conditionally files)
  (load files environment))

(write-string "\n\n---- Loading compile-time files ----")

(scratch/sf-and-load '("base/pkging") system-global-environment)

(scratch/sf-and-load '("base/switch" "base/macros" "base/hashtb" "base/decls"
				     "base/pmlook" "base/pmpars")
		     compiler-package)

(scratch/sf-and-load '("machines/butterfly/assmd")
		     (access bit-package compiler-package))

(scratch/sf-and-load '("back/syntax" "machines/butterfly/coerce" "back/asmmac"
				     "machines/butterfly/insmac")
		     (access lap-syntax-package compiler-package))

(if (access compiler:enable-expansion-declarations? compiler-package)
    (begin
      (scratch/sf-and-load '("base/scode" "base/pmerly")
			   compiler-package)
      (scratch/sf-and-load '("machines/butterfly/inerly" "back/syerly")
			   (access lap-syntax-package compiler-package))
      (fluid-let ((load-noisily? false)
		  (*rep-current-syntax-table*
		   (access early-syntax-table compiler-package)))
	(for-each (lambda (name)
		    (write-string "\nPre-loading instruction set from ")
		    (write name)
		    (load (string-append "machines/butterfly/" name ".scm")
			  (access lap-syntax-package compiler-package))
		    (write-string " -- done"))
		  '("instr1" "instr2" "instr3" "instr4")))))
