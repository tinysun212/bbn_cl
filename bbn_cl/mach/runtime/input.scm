;;; -*-Scheme-*-
;;;
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

;;;; Input

(declare (usual-integrations))

;;;; Input Ports

(define input-port-tag
  "Input Port")

(define (input-port? object)
  (and (environment? object)
       (not (lexical-unreferenceable? object ':type))
       (eq? (access :type object) input-port-tag)))

(define eof-object
  "EOF Object")

(define (eof-object? object)
  (eq? object eof-object))

(define *current-input-port*)

(define (current-input-port)
  *current-input-port*)

(define (with-input-from-port port thunk)
  (if (not (input-port? port)) (error "Bad input port" port))
  (fluid-let ((*current-input-port* port))
    (thunk)))

(define (with-input-from-file input-specifier thunk)
  (let ((port))
    (dynamic-wind (lambda ()
		    (set! port (open-input-file input-specifier)))
		  (lambda ()
		    (with-input-from-port port thunk))
		  (lambda ()
		    (close-input-port port)))))

(define (call-with-input-file input-specifier receiver)
  (let ((port (open-input-file input-specifier)))
    (let ((value (receiver port)))
      (close-input-port port)
      value)))

(define (close-input-port port)
  ((access :close port)))

;;;; Console Input Port

(define console-input-port)
(let ()

(define tty-read-char
  (make-primitive-procedure 'TTY-READ-CHAR))

(define tty-read-char-immediate
  (make-primitive-procedure 'TTY-READ-CHAR-IMMEDIATE))

(define tty-read-char-ready?
  (make-primitive-procedure 'TTY-READ-CHAR-READY?))

(define tty-read-finish
  (make-primitive-procedure 'TTY-READ-FINISH))

(define (read-start-hook)
  'DONE)

(define (read-finish-hook)
  'DONE)

(set! console-input-port
      (make-environment

(define :type input-port-tag)

(define (:print-self)
  (unparse-with-brackets
   (lambda ()
     (write-string "Console input port"))))

(define (:close)
  'DONE)

(define read-char-buffer #f)
(define unread-char-buffer #f)


(define (:read-char)
  (if unread-char-buffer
      (let ((result unread-char-buffer))
	(set! unread-char-buffer #f)
	(set! read-char-buffer result)
	result)
      (let ((result (tty-read-char)))
	(set! read-char-buffer result)
	result)))

(define (:unread-char)
  (set! unread-char-buffer read-char-buffer)
  unread-char-buffer)

(define (:peek-char)
  (:read-char)
  (:unread-char))

(define :discard-char :read-char)

(define (:read-string delimiters)
  (define (loop)
    (if (char-set-member? delimiters (:peek-char))
	'()
	(let ((char (:read-char)))
	  (cons char (loop)))))
  (list->string (loop)))

(define (:discard-chars delimiters)
  (define (loop)
    (if (not (char-set-member? delimiters (:peek-char)))
	(begin (:discard-char)
	       (loop))))
  (loop))

(define (:peek-char-immediate)
  (:read-char-immediate)
  (:unread-char))

(define (:read-char-immediate)
  (if unread-char-buffer
      (let ((result unread-char-buffer))
	(set! unread-char-buffer #f)
	(set! read-char-buffer result)
	result)
      (let ((result (tty-read-char-immediate)))
	(set! read-char-buffer result)
	result)))

(define (:char-ready? delay)
  (or unread-char-buffer (tty-read-char-ready? delay)))

(define (:read-start!)
  (read-start-hook))

(define :read-finish!
  (let ()
    (define (read-finish-loop)
      (if (and (:char-ready? 0)
	       (char-whitespace? (:peek-char)))
	  (begin (:discard-char)
		 (read-finish-loop))))
    (lambda ()
      (tty-read-finish)
;;;;      (read-finish-loop)   ;;;; pulled out because :char-ready? flushes typeahead [las]
      (read-finish-hook))))

;;; end CONSOLE-INPUT-PORT.
))

)

(set! *current-input-port* console-input-port)

;;;; File Input Ports

(define open-input-file)
(let ()

(define file-fill-input-buffer
  (make-primitive-procedure 'FILE-FILL-INPUT-BUFFER))

(define file-fill-input-buffer-offset
  (make-primitive-procedure 'FILE-FILL-INPUT-BUFFER-OFFSET))

(define file-length
  (make-primitive-procedure 'FILE-LENGTH))

(define file-port-buffer-size
  512)

(set! open-input-file
(named-lambda (open-input-file filename)
  (let ((file-channel ((access open-input-channel primitive-io)
		       (canonicalize-input-filename filename))))

(define :type input-port-tag)

(define (:print-self)
  (unparse-with-brackets
   (lambda ()
     (write-string "Buffered input port for file: ")
     (write ((access channel-name primitive-io) file-channel)))))

(define (:pathname)
  (->pathname filename))

(define (:truename)
  (->pathname ((access channel-name primitive-io) file-channel)))

(define (:length)
  (file-length file-channel))

(define buffer false)
(define start-index 0)
(define end-index -1)

(define (refill-buffer!)
  (if (not buffer) (set! buffer (string-allocate file-port-buffer-size)))
  (set! start-index 1)
  (set! end-index (1+ (file-fill-input-buffer-offset file-channel buffer 1)))
  (= end-index 1))

(declare (integrate buffer-ready?))

(define (buffer-ready?)
  (and (not (= end-index 1))
       (not (refill-buffer!))))

(define (:char-ready? delay)
  (or (< start-index end-index)
      (buffer-ready?)))

(define (:close)
  (set! end-index 0)
  (set! buffer false)
  ((access close-physical-channel primitive-io) file-channel))

(define (:peek-char)
  (if (< start-index end-index)
      (string-ref buffer start-index)
      (and (buffer-ready?)
	   (string-ref buffer 1))))

(define (:discard-char)
  (set! start-index (1+ start-index)))

(define (:read-char)
  (if (< start-index end-index)
      (string-ref buffer (set! start-index (1+ start-index)))
      (and (buffer-ready?)
	   (begin (set! start-index 2)
		  (string-ref buffer 1)))))

(define (:unread-char)
  (set! start-index (-1+ start-index)))

(define (:read-string delimiters)
  (define (loop)
    (let ((index
	   (substring-find-next-char-in-set buffer start-index end-index
					    delimiters)))
      (if index
	  (substring buffer (set! start-index index) index)
	  (let ((head (substring buffer start-index end-index)))
	    (if (refill-buffer!)
		head
		(let ((tail (loop))
		      (head-length (string-length head)))
		  (let ((result (string-allocate (+ head-length
						    (string-length tail)))))
		    (substring-move-right! head 0 head-length
					   result 0)
		    (substring-move-right! tail 0 (string-length tail)
					   result head-length)
		    result)))))))
  (and (or (< start-index end-index)
	   (buffer-ready?))
       (loop)))

(define (:discard-chars delimiters)
  (define (loop)
    (let ((index
	   (substring-find-next-char-in-set buffer start-index end-index
					    delimiters)))
      (cond (index (set! start-index index))
	    ((not (refill-buffer!)) (loop)))))
  (if (or (< start-index end-index)
	  (buffer-ready?))
      (loop)))

(define (:rest->string)
  (define (read-rest)
    (set! end-index 0)
    (loop))

  (define (loop)
    (let ((buffer (string-allocate file-port-buffer-size)))
      (let ((n (file-fill-input-buffer file-channel buffer)))
	(cond ((zero? n) '())
	      ((< n file-port-buffer-size)
	       (set-string-length! buffer n)
	       (list buffer))
	      (else (cons buffer (loop)))))))

  (if (zero? end-index)
      (error "End of file -- :REST->STRING"))
  (cond ((= -1 end-index)
	 (let ((l (:length)))
	   (if l
	       (let ((buffer (string-allocate l)))
		 (set! end-index 0)
		 (file-fill-input-buffer file-channel buffer)
		 buffer)
	       (apply string-append (read-rest)))))
	((< start-index end-index)
	 (let ((first (substring buffer start-index end-index)))
	   (apply string-append
		  (cons first
			(read-rest)))))
	(else
	 (apply string-append (read-rest)))))

(the-environment))))

)

;;;; String Input Ports

(define (with-input-from-string string thunk)
  (fluid-let ((*current-input-port* (string->input-port string)))
    (thunk)))

(define (string->input-port string #!optional start end)
  (cond ((unassigned? start)
	 (set! start 0)
	 (set! end (string-length string)))
	((unassigned? end)
	 (set! end (string-length string))))

(define :type input-port-tag)

(define (:print-self)
  (unparse-with-brackets
   (lambda ()
     (write-string "Input port for string"))))

(define (:char-ready? delay)
  (< start end))

(define (:close) 'DONE)

(define (:peek-char)
  (and (< start end)
       (string-ref string start)))

(define (:discard-char)
  (set! start (1+ start)))

(define (:read-char)
  (and (< start end)
       (string-ref string (set! start (1+ start)))))

(define (:read-string delimiters)
  (and (< start end)
       (let ((index
	      (or (substring-find-next-char-in-set string start end delimiters)
		  end)))
	 (substring string (set! start index) index))))

(define (:discard-chars delimiters)
  (if (< start end)
      (set! start
	    (or (substring-find-next-char-in-set string start end delimiters)
		end))))

;;; end STRING->INPUT-PORT.
(the-environment))

;;;; Input Procedures

(define (peek-char #!optional port)
  (cond ((unassigned? port) (set! port *current-input-port*))
	((not (input-port? port)) (error "Bad input port" port)))
  (or ((if (lexical-unreferenceable? port ':peek-char-immediate)
	   (access :peek-char port)
	   (access :peek-char-immediate port)))
      eof-object))

(define (read-char #!optional port)
  (cond ((unassigned? port) (set! port *current-input-port*))
	((not (input-port? port)) (error "Bad input port" port)))
  (or ((if (lexical-unreferenceable? port ':read-char-immediate)
	   (access :read-char port)
	   (access :read-char-immediate port)))
      eof-object))

(define (read-string delimiters #!optional port)
  (cond ((unassigned? port) (set! port *current-input-port*))
	((not (input-port? port)) (error "Bad input port" port)))
  (or ((access :read-string port) delimiters)
      eof-object))

(define (read #!optional port)
  (cond ((unassigned? port) (set! port *current-input-port*))
	((not (input-port? port)) (error "Bad input port" port)))
  (if (not (lexical-unreferenceable? port ':read-start!))
      ((access :read-start! port)))
  (let ((object ((access *parse-object parser-package) port)))
    (if (not (lexical-unreferenceable? port ':read-finish!))
	((access :read-finish! port)))
    object))

;;; **** The DELAY option for this operation works only for the
;;; console port.  Since it is a kludge, it is probably OK.

(define (char-ready? #!optional port delay)
  (cond ((unassigned? port) (set! port *current-input-port*))
	((not (input-port? port)) (error "Bad input port" port)))
  (cond ((unassigned? delay) (set! delay 0))
	((not (and (integer? delay) (>= delay 0))) (error "Bad delay" delay)))
  ((access :char-ready? port) delay))

(define (read-char-no-hang #!optional port)
  (cond ((unassigned? port) (set! port *current-input-port*))
	((not (input-port? port)) (error "Bad input port" port)))
  (and ((access :char-ready? port) 0)
       (read-char port)))

;;; Redefined by cl-io-mode

(define (clear-input #!optional port)
  #f)


(define load)
(define load-noisily)
(define load-noisily? #f)  ; For making things noisy that are normally quiet.
(define load-quietly? #f)  ; For making things quiet that are normally noisy.
(define load-dont-error? #f) ; For stopping errors
(define *commonlisp-messages* #f) ; For commonlisp-style messages

(define read-file)


(define *package* #f)

(define *load-source-filename-extension* "scm")

(let ()

(define eof-error-code ((make-primitive-procedure 'make-io-error-code) #xE0F))

(define default-pathname
  (make-pathname false false false false false 'NEWEST))

;;; This crufty piece of code, once it decides which file to load,
;;; does `file-exists?' on that file at least three times!!

(define (basic-load filename environment)
  (define (kernel pathname)
    (let ((pathname
	   (or (pathname->input-truename pathname)
	       (let ((pathname (merge-pathnames pathname default-pathname)))
		 (if (pathname-type pathname)
		     (pathname->input-truename pathname)
		     (let* ((namelist	; Of the files that exist, will be sorted in "pecking order".
			     (delete '() 
				     (mapcar
				      (lambda (type)
					(pathname->input-truename
					 (pathname-new-type pathname type)))
				      (list "com" "bin" *load-source-filename-extension*))))
			    (most-recent ; Find most recent, with ties honoring pecking order.
			     (cond
			      ((null? namelist)
			       '())
			      ((= (length namelist) 1)
			       (car namelist))
			      (else
			       (let loop ((l namelist) (current (car namelist)))
				 (cond
				  ((null? l)
				   current)
				  ((> (file-write-date (car l))
				      (file-write-date current))
				   (loop (cdr l) (car l)))
				  (else
				   (loop (cdr l) current)))))))
			    (warning-list ; FInd all names to left of most recent.
			     (let loop ((l namelist))
			       (if (or (null? l)
				       (eq? most-recent (car l)))
				   '()
				   (cons (car l) (loop (cdr l)))))))
		       (if warning-list
			   (begin
			     (princ ";;; Warning: Loading ")
			     (princ (pathname->string most-recent))
			     (princ " because it is more recent than ")
			     (if (= (length warning-list) 1)
				 (princ (pathname->string (car warning-list)))
				 (princ (mapcar pathname->string
						warning-list)))))
		       most-recent)))
	       load-dont-error?
	       (error "No such file" pathname)))
	  (port (rep-output-port)))
      (if (not load-quietly?)
	  (begin (newline port)
		 (if *commonlisp-messages* (write-string ";;; " port))
		 (write-string "Loading " port)
		 (write (pathname->string pathname) port)))
      (cond ((eq? pathname load-dont-error?)
	     load-dont-error?)
	    ((and is-a-butterfly?
		  (equal? (pathname-type pathname) *load-source-filename-extension*))
	     (sexp-load pathname))
	    ((and is-a-butterfly? 
		  (member (pathname-type pathname) '("bin" "com")))
	     (scode-load pathname))
	    ((call-with-input-file
		 pathname
	       (lambda (port) (= 250 (char->ascii (peek-char port)))))
	     (scode-load pathname))
	    (else
	     (sexp-load pathname)))))

  ;; Only need to save commonlisp status if are really in
  ;; commonlisp, as indicated by the state of *package*
  
  (define (with-saved-commonlisp-status thunk)
    (if *package*
	(fluid-let ((*package* *package*))
	  (let ((old-mode (touch-mode)))
	    (dynamic-wind
	     (lambda () #f)
	     thunk
	     (lambda ()
	       (syntaxer-proclaim!
		(list 'insert-touches old-mode))))))
	(thunk)))

  (define (sexp-load filename)
    (call-with-input-file filename
      (lambda (port)
	(with-saved-commonlisp-status
	 (lambda ()
	   (define (load-loop previous-object)
	     (let ((object (read port)))
	       (if (eof-object? object)
		   previous-object
		   (let ((value (eval object environment)))
		     (if load-noisily? (begin (newline) (write value)))
		     (load-loop value)))))
	   (load-loop *the-non-printing-object*))))))

  (define (scode-load pathname)
    (call-with-fasl-input-file pathname
      (lambda (fd)
	(with-saved-commonlisp-status
	 (lambda ()
	   (let loop ((scode-object (fasload-fd fd *package*))
		      (values '()))
	     (if (eq? scode-object eof-error-code)
		 (reverse! values)
		 (let ((value (scode-eval scode-object environment))) ; eval may change *package*, so force ordering
		   (loop (fasload-fd fd *package*)
			 (cons value
			       values))))))))))

  (define prim-file-write-date (make-primitive-procedure 'cl-file-write-date))

  (define (file-write-date file)
    (prim-file-write-date 
     (cond
      ((string? file)
       file)
      ((pathname? file)
       (pathname->string file))
      (else
       (->string file)))))

  (let ((result 
	 (let load-loop ((files (stickify-input-filenames filename false)))
	   (if (null? files)
	       '()
	       (let ((result1 (kernel (car files))))
		 (cons result1 (load-loop (cdr files))))))))
    (if (eq? (car result) load-dont-error?)
	load-dont-error?
	*the-non-printing-object*)))

(set! load
  (named-lambda (load filename #!optional environment)
    (if (unassigned? environment) (set! environment (rep-environment)))
    (basic-load filename environment)))

(set! load-noisily
  (named-lambda (load-noisily filename #!optional environment)
    (if (unassigned? environment) (set! environment (rep-environment)))
    (fluid-let ((load-noisily? true))
      (basic-load filename environment))))

(set! read-file
  (named-lambda (read-file filename)
    (let ((name (pathname->input-truename
		 (merge-pathnames (->pathname filename) default-pathname))))
      (if name
	  (call-with-input-file name
	    (access *parse-objects-until-eof parser-package))
	  (error "Read-file: No such file" name)))))
)

(define (stickify-input-filenames filename/s default-pathname)
  (map (if default-pathname
	   (lambda (filename)
	     (merge-pathnames (->pathname filename) default-pathname))
	   ->pathname)
       (if (pair? filename/s)
	   filename/s
	   (list filename/s))))

#|(define (stickify-input-filenames filename/s default-pathname)
  (let loop
      ((filenames 
	(if (pair? filename/s)
	    filename/s
	    (list filename/s)))
       (default-pathname default-pathname))
    (let ((pathname
	   (let ((pathname (->pathname (car filenames))))
	     (if default-pathname
		 (merge-pathnames pathname default-pathname)
		 pathname))))
      (cons pathname
	    (if (pair? (cdr filenames))
		(loop (cdr filenames) pathname)
		'())))))|#

;;;; Serializing utility used by fasload and fasdump.
;;
;; Both fasload and fasdump must be serialized because:
;;
;; - fasload plays with the obarray (and/or Common Lisp package system),
;; and having two processors doing this simultaneously would be
;; disastrous.
;;
;; - fasdump destructively copies an object, and continuing computation
;; while this is going on would make broken hearts appear everywhere.
;; Before returning, fasdump fixes all the objects it has clobbered,
;; so proceeding when it is done is fine.

(define (serialize-operation operation)
  (let ((start-synch (make-synchronizer))
	(end-synch (make-synchronizer)))
    (global-interrupt
     2
     (lambda (int-code int-mask)
       (await-synchrony start-synch)
       (await-synchrony end-synch))
     (lambda () #t))
    (await-synchrony start-synch)
    (let ((value (operation)))
      (await-synchrony end-synch)
      value)))


(define fasload)
(define fasload-fd)
(define call-with-fasl-input-file)
(define fasload-multiple)
(define fassize)
(define fasseek (make-primitive-procedure 'fasseek))
(define faspos (make-primitive-procedure 'faspos))
(define io-error-code? (make-primitive-procedure 'io-error-code-p))
(define make-io-error-code (make-primitive-procedure 'make-io-error-code))
(define eof-error-code (make-io-error-code #xE0F))

(let ()

(define fasopen-read (make-primitive-procedure 'fasopen-read))
(define binary-fasload-fd (make-primitive-procedure 'binary-fasload-fd))
(define fasclose (make-primitive-procedure 'fasclose))

(define default-pathname
  (make-pathname false false false false "bin" 'NEWEST))

(define binary-fasload
  (make-primitive-procedure 'BINARY-FASLOAD))

(define (fasload-kernel action prefix)
  (lambda (filename #!optional cl-pkg)
    (if (unassigned? cl-pkg) (set! cl-pkg *package*))
    (let ((filename (canonicalize-input-filename
		     (merge-pathnames (->pathname filename)
				      default-pathname))))
      (let ((port (rep-output-port))
	    (string
	     (if load-quietly?
		 ""
		 (with-output-to-string
		   (lambda ()
		     (newline)
		     (if *commonlisp-messages* (write-string ";;; "))
		     (write-string prefix)
		     (write filename))))))
	(call-with-fasl-input-file
	 filename
	 (lambda (fd)
	   (serialize-operation
	    (lambda ()
	      (if (not load-quietly?)
		  (write-string string port))
	      (let ((value (action fd cl-pkg)))
		(if (not load-quietly?)
		    (write-string " -- done" port))
		value)))))))))
  

(set! fasload
      (fasload-kernel binary-fasload-fd
		      "FASLoading "))

(set! fasload-multiple
      (fasload-kernel (lambda (fd cl-pkg)
			(let loop ((acc '()))
			  (let ((scode (binary-fasload-fd fd cl-pkg)))
			    (if (eq? scode eof-error-code)
				(reverse! acc)
				(loop (cons scode acc))))))
		      "FASLoading-Multiple "))

(set! fasload-fd
      (named-lambda (fasload-fd fd #!optional cl-pkg)
	(if (unassigned? cl-pkg) (set! cl-pkg *package*))
	(let ((port (rep-output-port))
	      (string
	       (if (not load-noisily?)
		   ""
		   (with-output-to-string
		     (lambda ()
		       (newline)
		       (if *commonlisp-messages* (write-string ";;; "))
		       (write-string "FASLoading From File Descriptor ")
		       (write fd))))))
	  (serialize-operation
	   (lambda ()
	     (if load-noisily?
		 (write-string string port))
	     (let ((value (binary-fasload-fd fd cl-pkg)))
	       (if load-noisily?
		   (write-string " -- done" port))
	       value))))))

(set! call-with-fasl-input-file
      (named-lambda (call-with-fasl-input-file file proc)
	(let ((fd (fasopen-read (canonicalize-input-filename
				 (merge-pathnames (->pathname file)
						  default-pathname)))))
	  (if (io-error-code? fd)
	      (error "I/O error in fasl open for read" (list fd file))
	      (dynamic-wind (lambda () 'DONE)
			    (lambda () (proc fd))
			    (lambda () (fasclose fd)))))))

(set! fassize
(named-lambda (fassize file)
  (call-with-fasl-input-file file
    (lambda (fd)
      (let loop ((old-pos 0)
		 (object-no 1)
		 (size-list '()))
	(let ((status (fasseek fd object-no)))
	  (if (io-error-code? status)
	      (cond
	       ((eq? status eof-error-code)
		(reverse! size-list))
	       (else
		(error "I/O error in fassize" (list status file))))
	      (let ((pos (faspos fd)))
		(loop pos (1+ object-no) (cons (- pos old-pos) size-list))))))))))

)

(define transcript-on
  (let ((photo-open (make-primitive-procedure 'PHOTO-OPEN)))
    (named-lambda (transcript-on filename)
      (if (not (photo-open (canonicalize-output-filename filename)))
	  (error "Transcript file already open: TRANSCRIPT-ON" filename))
      *the-non-printing-object*)))

(define transcript-off
  (let ((photo-close (make-primitive-procedure 'PHOTO-CLOSE)))
    (named-lambda (transcript-off)
      (if (not (photo-close))
	  (error "Transcript file already closed: TRANSCRIPT-OFF"))
      *the-non-printing-object*)))
