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

;;;; Filename Completion

(declare (usual-integrations))

;;; *PATHNAME-CACHE* is a special optimization to assist completion
;;; when the directory is located on a slow device (e.g. floppy disk).
;;; It should be fluid-bound to #F over a short period of time, during
;;; which no directory modifications will take place (thereby
;;; invalidating the cached information).  To use complete-pathname
;;; without the cache, fluid-bind *PATHNAME-CACHE* to #T.

(define *pathname-cache*)
(define complete-pathname)
(define pathname-newest)
(define pathname-completions)
(let ()

(set! complete-pathname
  (named-lambda (complete-pathname pathname default-pathname
				   if-unambiguous if-ambiguous if-not-found)
    (define ((try-component component if-win) pathnames)
      (cond ((all-same-component? component pathnames)
	     (if-win pathnames))
	    ((component default-pathname)
	     (let ((pathnames* (filter-same-component component
						      default-pathname
						      pathnames)))
	       (if (null? pathnames*)
		   (return-ambiguous pathnames)
		   (if-win pathnames*))))
	    (else
	     (return-ambiguous pathnames))))

    (define (return-greatest-version pathnames)
      (let ((pathname (greatest-version pathnames)))
	(if pathname
	    (if-unambiguous pathname)
	    (return-ambiguous pathnames))))

    (define (return-ambiguous pathnames)
      (greatest-common-prefix pathnames if-ambiguous))

    (let ((pathnames (pathname-completions pathname default-pathname)))
      (cond ((null? pathnames) (if-not-found))
	    ((null? (cdr pathnames)) (if-unambiguous (car pathnames)))
	    (else
	     ((try-component pathname-name
			     (try-component pathname-type
					    return-greatest-version))
	      pathnames))))))

(set! pathname-newest
  (named-lambda (pathname-newest pathname)
    (let ((pathnames
	   (versions-of-pathname (pathname->absolute-pathname pathname))))
      (and (not (null? pathnames))
	   (greatest-version pathnames)))))

(define (all-same-component? component pathnames)
  (let ((name (component (car pathnames))))
    (define (loop pathnames)
      (or (null? pathnames)
	  (and (eqv? name (component (car pathnames)))
	       (loop (cdr pathnames)))))
    (loop (cdr pathnames))))

(define (filter-same-component component pathname pathnames)
  (list-transform-positive pathnames
    (let ((key (component pathname)))
      (lambda (pathname)
	(eqv? key (component pathname))))))

(define (greatest-version pathnames)
  (define (find-numeric-version pathnames*)
    (and (not (null? pathnames*))
	 ((let ((version (pathname-version (car pathnames*))))
	   (if (and (integer? version) (positive? version))
	       (outer-loop (car pathnames*) version)
	       find-numeric-version))
	  (cdr pathnames*))))

  (define (outer-loop pathname greatest-version)
    (define (inner-loop pathnames)
      (if (null? pathnames)
	  pathname
	  ((let ((version (pathname-version (car pathnames))))
	     (if (and (integer? version) (> version greatest-version))
		 (outer-loop (car pathnames) version)
		 inner-loop))
	   (cdr pathnames))))
    inner-loop)

  (or (find-numeric-version pathnames)
      (if (null? (cdr pathnames))
	  (car pathnames)
	  (let loop ((pathnames pathnames))
	    (and (not (null? pathnames))
		 (if (not (pathname-version (car pathnames)))
		     (car pathnames)
		     (loop (cdr pathnames))))))))

(define (greatest-common-prefix pathnames receiver)
  (define (loop pathnames string index)
    (if (null? pathnames)
	(receiver string 0 index)
	(let ((string* (pathname->string (car pathnames))))
	  (let ((index* (string-match-forward string string*)))
	    (if (< index* index)
		(loop (cdr pathnames) string* index*)
		(loop (cdr pathnames) string index))))))
  (let ((string (pathname->string (car pathnames))))
    (loop (cdr pathnames) string (string-length string))))

;;;; Pathname Matching

(set! pathname-completions
  (named-lambda (pathname-completions pathname default-pathname)
    (matching-pathnames
     (pathname->absolute-pathname (merge-pathnames pathname default-pathname))
     pathname)))

(define (matching-pathnames directory-pathname name-pathname)
  (let ((directory (pathname-directory-path directory-pathname)))
    (let ((directory-string (pathname->string directory))
	  (name-string (pathname-name-string name-pathname)))
      (define (filter-matching-pathnames filenames pathnames)
	(cond ((null? filenames) '())
	      ((string-prefix? name-string (car filenames))
	       (cons (car pathnames)
		     (filter-matching-pathnames (cdr filenames)
						(cdr pathnames))))
	      (else
	       (filter-matching-pathnames (cdr filenames)
					  (cdr pathnames)))))

      (define (generate-pathnames receiver)
	(let ((filenames
	       (generate-matching-filenames directory-string name-string)))
	  (receiver filenames
		    (map (lambda (filename)
			   (merge-pathnames directory
					    (string->pathname filename)))
			 filenames))))

      (cond ((unassigned? *pathname-cache*)
	     (generate-pathnames (lambda (filenames pathnames) pathnames)))
	    ((and (vector? *pathname-cache*)
		  (string=? (vector-ref *pathname-cache* 0)
			    directory-string)
		  (string-prefix? (vector-ref *pathname-cache* 1) name-string))
	     (filter-matching-pathnames (vector-ref *pathname-cache* 2)
					(vector-ref *pathname-cache* 3)))
	    ((or (not *pathname-cache*)
		 (vector? *pathname-cache*))
	     (set! *pathname-cache*
		   (generate-pathnames
		    (lambda (filenames pathnames)
		      (vector directory-string name-string
			      filenames pathnames))))
	     (vector-ref *pathname-cache* 3))
	    (else
	     (generate-pathnames (lambda (filenames pathnames) pathnames)))))))

(define (versions-of-pathname pathname)
  (let ((directory (pathname-directory-path pathname))
	(name (pathname-name pathname))
	(type (pathname-type pathname)))
    (map (lambda (pathname)
	   (merge-pathnames directory pathname))
	 (list-transform-positive
	  (map string->pathname 
	       (generate-matching-filenames
		(pathname->string directory)
		(pathname-extract-string pathname 'NAME 'TYPE)))
	  (lambda (pathname)
	    (and (equal? (pathname-name pathname) name)
		 (equal? (pathname-type pathname) type)))))))

(define open-directory (make-primitive-procedure 'OPEN-DIRECTORY 1))
(define directory-read (make-primitive-procedure 'DIRECTORY-READ 0))

(define (generate-matching-filenames directory name-prefix)
  (define (loop name)
    (if name
	(let ((rest (loop (directory-read))))
	  (if (string-prefix? name-prefix name)
	      (cons name rest)
	      rest))
	'()))
  (loop (open-directory directory)))

)
