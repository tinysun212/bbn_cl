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
(proclaim '(insert-touches nil))

(defsetf package-name %set-package-name)
(defsetf package-nicknames %set-package-nicknames)
(defsetf package-use-list %set-package-use-list)
(defsetf package-used-by-list %set-package-used-by-list)
(defsetf package-shadowing-symbols %set-package-shadowing-symbols)
(defsetf package-tables %set-package-tables)
(defsetf package-internal-symbols %set-package-internal-symbols)
(defsetf package-external-symbols %set-package-external-symbols)
(defsetf pkg-gethash %pkg-sethash)

