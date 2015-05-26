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
;;;;;; -*- Scheme -*-

(write-string "\n\n---- Deleting old files ----")
(for-each (lambda (subdirectory)
	    (for-each delete-file
		      (directory-read (string-append subdirectory "/*.bin")))
	    (for-each delete-file
		      (directory-read (string-append subdirectory "/*.ext"))))
	  scratch/subdirectories)

(write-string "\n\n---- Sorting files ----")
(let ((source-files
       ((access source-files-by-rank decls-package compiler-package))))
  (write-string "\n\n---- Starting pass 1 ----")
  (sf source-files))

(write-string "\n\n---- Starting pass 2 ----")
(sf ((access source-files-with-circular-dependencies decls-package
	     compiler-package)))