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
;;;(proclaim '(insert-touches nil))

(export '(
	  *default-pathname-defaults* pathname truename merge-pathnames
	  make-pathname pathnamep parse-namestring 
	  pathname-host pathname-device pathname-directory pathname-name
	  pathname-type pathname-version namestring file-namestring 
	  directory-namestring host-namestring enough-namestring 
	  user-homedir-pathname rename-file delete-file probe-file
	  file-author file-write-date 
	  *load-verbose*
	  directory
	  load
))

;;;
;;; Part the First: construct CL pathname system from Scheme's.
;;;

(cl-define %make-pathname (access make-pathname ()))

(defvar *default-pathname-defaults*)
(setq *default-pathname-defaults*
      (%make-pathname nil nil nil nil nil nil))

(cl-define escaped-symbol-name?
  (access escaped-symbol-name? cl-unparser-package))

(defun symbol->filename-string (s)
  (let ((name (symbol-name s)))
    (if (escaped-symbol-name? name)
	name
	(string-downcase name))))

(defun pathname (x)
  (typecase x
    (pathname x)
    (string (string->pathname x))
    (symbol (string->pathname (symbol->filename-string x)))
    (stream 
     (let ((p (stream-pathname x)))
       (or p
	   (error "No pathname associated with stream ~a" x))))
    (t (error "Cannot coerce ~a to a pathname~%" x))))

(defun truename (x)
  (let* ((path (pathname x))
	 (name (pathname->input-truename path))); CL always uses "input" style truename
    (or name
	(error "File ~A does not exist" path))))

(defun merge-pathnames (pathname &optional (defaults *default-pathname-defaults*) default-version)
  (setq pathname (pathname pathname))
  (setq defaults (pathname defaults))
  (%make-pathname
   (or (pathname-host pathname) (pathname-host defaults))
   (if (and (pathname-host pathname)
	    (null (pathname-device pathname))
	    (equal (pathname-host pathname) (pathname-host defaults)))
       (pathname-device defaults)
       nil)
   (or (pathname-directory pathname) (pathname-directory defaults))
   (or (pathname-name pathname) (pathname-name defaults))
   (or (pathname-type pathname) (pathname-type defaults))
   (or (pathname-version pathname)
       (if (pathname-name pathname)
	   default-version
	   (or (pathname-version defaults) default-version)))))

(defun make-pathname (&key (host nil hostp) (device nil devicep) (directory nil directoryp)
			   (name nil namep) (type nil typep) (version nil versionp)
			   defaults)
  (when defaults (setq defaults (pathname defaults)))
  (let ((default-host
	  (if defaults
	      (pathname-host defaults)
	      (pathname-host *default-pathname-defaults*))))
    (unless hostp (setq host default-host))
    (%make-pathname
     (touch host)
     (if devicep
	 (touch device)
	 (when (and hostp (equal host default-host))
	       (when defaults
		     (pathname-device defaults))))
     (if directoryp (touch directory) (when defaults (pathname-directory defaults)))
     (if namep (touch name) (when defaults (pathname-name defaults)))
     (if typep (touch type) (when defaults (pathname-type defaults)))
     (if versionp
	 (touch version)
	 (unless namep
		 (when defaults (pathname-version defaults)))))))

;; Pathnamep is in clchap6.scm so that equal will work early.

(defun parse-namestring (x &optional host (defaults *default-pathname-defaults*) &key (start 0) end junk-allowed)
  (unless host (setq host (pathname-host defaults)))
  (typecase x
    (string)
    (symbol 
     (setq x (symbol-name x)))
    ((pathname stream)
     (setq x (pathname x))
     (if (equal host (pathname-host x))
	 (return-from parse-namestring (values x start))
	 (error "Unmatching hosts: ~A with ~A" (pathname-host x) host)))
    (t (error "~a must be a string, symbol, stream, or pathname" x)))
  (let ((p (if (and (= start 0)
		    (or (null end) (= end (length x))))
	       (string->pathname x)
	       (string-pathname (substring x start end)))))
    (unless (equal host (pathname-host p))
	    (error "Unmatching hosts: ~A with ~A" (pathname-host p) host))
    (values p end)))

(cl-define %pathname-host pathname-host)
(cl-define %pathname-device pathname-device)
(cl-define %pathname-directory pathname-directory)
(cl-define %pathname-name pathname-name)
(cl-define %pathname-type pathname-type)
(cl-define %pathname-version pathname-version)

(defun pathname-host (x) (%pathname-host (pathname x)))
(defun pathname-device (x) (%pathname-device (pathname x)))
(defun pathname-directory (x) (%pathname-directory (pathname x)))
(defun pathname-name (x) (%pathname-name (pathname x)))
(defun pathname-type (x) (%pathname-type (pathname x)))
(defun pathname-version (x) (%pathname-version (pathname x)))

(defun namestring (x)
  (pathname->string (pathname x)))

(defun file-namestring (x)
  (setq x (pathname x))
  (pathname-unparse-name (pathname-name x) (pathname-type x) (pathname-version x)))

(cl-define %unparse-directory unparse-directory)
(cl-define %unparse-host unparse-host)

(defun directory-namestring (x)
  (%unparse-directory (pathname-directory (pathname x)) ""))

(defun host-namestring (x)
  (%unparse-host (pathname-host (pathname x)) ""))

(defun enough-namestring (x &optional (defaults *default-pathname-defaults*))
  (setq x (pathname x))
  (%make-pathname
    (if (equal (pathname-host x) (pathname-host defaults)) nil (pathname-host x))
    (if (equal (pathname-device x) (pathname-device defaults)) nil (pathname-device x))
    (if (equal (pathname-directory x) (pathname-directory defaults)) nil (pathname-directory x))
    (if (equal (pathname-name x) (pathname-name defaults)) nil (pathname-name x))
    (if (equal (pathname-type x) (pathname-type defaults)) nil (pathname-type x))
    (if (equal (pathname-version x) (pathname-version defaults)) nil (pathname-version x))))

(defun user-homedir-pathname (&optional host)
  (truename (pathname "~/")))

;;;
;;; Part the Second: Rename, Delete, etc.
;;;

(cl-define %rename-file rename-file)

(defun rename-file (file new-name)
  (setq file (pathname file))
  (setq new-name (pathname new-name))
  (%rename-file file new-name)
  (values (merge-pathnames new-name file)
	  (truename file)
	  (truename new-name)))

(cl-define %delete-file delete-file)

(defun delete-file (file)
  (setq file (pathname file))
  (%delete-file file))

(defun probe-file (file)
  (setq file (pathname file))
  (if (file-exists? file)
      (truename file)
      nil))

(cl-define cl-file-write-date (make-primitive-procedure 'cl-file-write-date))

(cl-define (file-write-date file-specifier)
	   (cl-file-write-date (namestring (truename (pathname file-specifier)))))

(cl-define cl-file-author (make-primitive-procedure 'cl-file-author))

(cl-define (file-author file-specifier)
	   (cl-file-author (namestring (truename (pathname file-specifier)))))

;;; Loading files

(cl-define scm-load load)
(cl-define scm-load-noisily load-noisily)

(defvar *load-verbose*)
(setq *load-verbose* nil)

(defvar dont-error-flag)
(setq dont-error-flag '(dont-error-flag))

(defun cl-load (filename &key (verbose *load-verbose*) (print nil) (if-does-not-exist t))
  (let ((file (merge-pathnames (pathname filename)
			       *default-pathname-defaults*)))
    (fluid-let ((load-quietly? t)
		(load-dont-error? dont-error-flag))
	     (if verbose
		 (format t ";;; Loading ~A ..." (truename file)))
	     (let ((load-result
		    (if print
			(scm-load-noisily file)
			(scm-load file))))
	       (if (eq load-result dont-error-flag)
		   (if if-does-not-exist
		       (error "Cannot load ~A, file does not exist." file)
		       nil)
		   file)))))
    
(cl-define directoryp (make-primitive-procedure 'cl-directory?))

(cl-define scan-directory (make-primitive-procedure 'cl-scan-directory))

(defun directory (pathname &key &allow-other-keys)
  (let ((name (namestring pathname)))
    (if (probe-file name)
	(if (directoryp name)
	    (map 'list #'pathname (scan-directory name))
	    (list (truename name)))
	nil)))

(cl-define pwd %pwd)
