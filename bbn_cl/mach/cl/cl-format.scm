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
(declare (usual-integrations))

(define *format-control-string* "")
(define *format-index* 0)
(define *format-length* 0)
(define *format-arguments* ())
(define *format-original-arguments* ())
(define *format-error*)
(define *format-escape*)
(define *format-colon-escape*)
(define *format-saved-output*)

(define the-start-keyword (keyword-intern "START"))
(define the-end-keyword (keyword-intern "END"))

(define make-string-output-stream (make-primitive-procedure 'cl-make-string-output-stream))
(define get-output-stream-string (make-primitive-procedure 'cl-get-output-stream-string))
(define string-open (make-primitive-procedure 'cl-string-open))
(define write-string (make-primitive-procedure 'cl-write-string))
(define write-char (make-primitive-procedure 'cl-write-char))

(define internal-write-string
  (let ((prim-internal-write-string (make-primitive-procedure 'cl-internal-write-string)))
    (lambda (string)
      (prim-internal-write-string string #f))))

(define internal-write-string-start-end
  (let ((prim-internal-write-string-start-end (make-primitive-procedure 'cl-internal-write-string-start-end)))
    (lambda (string start end)
      (prim-internal-write-string-start-end string #f start end))))

(define internal-write-char (make-primitive-procedure 'cl-internal-write-char))
(define force-output (make-primitive-procedure 'cl-force-output))

(define (flonum? x)
  ((symbol-function 'floatp) x))

(define (rational? x)
  ((symbol-function 'rationalp) x))

(define (coerce-to-float x)
  (if ((symbol-function 'ratiop) x)
      (/ (ratio-numer x) (ratio-denom x))
      ((symbol-function 'integer->float) x)))

(define (coerce-to-simple-string x)
  ((symbol-function 'coerce) x 'simple-string))

(define ratio-numer (make-primitive-procedure 'ratio-numerator))
(define ratio-denom (make-primitive-procedure 'ratio-denominator))

(define *format-dispatch-table*
  (make-vector 
   128 
   (lambda (colon atsign params command)
     (format-error "Illegal FORMAT command ~~~C" command))))

;;; This establishes the correct environment for processing
;;; an indirect control string.  CONTROL-STRING is the string to
;;; process, and THUNK does the processing.  It
;;; invariably will involve a call to SUB-FORMAT.

(define (format-with-control-string control-string thunk)
  ;; Might have to reduce commonlisp strings into scheme strings
  (let ((errorp (call-with-current-continuation
		 (lambda (cont)
		   (fluid-let ((*format-control-string* control-string)
			       (*format-length* (string-length control-string))
			       (*format-index* 0)
			       (*format-error* cont))
		     (thunk)
		     #f)))))
    (if errorp
	(*format-error*
	 (cons (list "~%While processing indirect control string:~%~S~%~V@T^"
		     *format-control-string*
		     (1+ *format-index*))
	       errorp)))))


;;; This rebinds *standard-output* to collect output
;;; in a string. 

(define (format-stringify-output thunk)
  (fluid-let ((*standard-output* (make-string-output-stream)))
    (thunk)
    (get-output-stream-string *standard-output*)))

;;; Pops an argument from the current argument list.  This is either the
;;; list of arguments given to the top-level call to FORMAT, or the argument
;;; list for the current iteration in a ~{~} construct.  An error is signalled
;;; if the argument list is empty.

(define (pop-format-arg)
  (if *format-arguments*
      (let ((return (car *format-arguments*)))
	(set! *format-arguments* (cdr *format-arguments*))
	return)
      (format-error "Missing argument")))

;;; Passes paramters to the continuation. If a parameter is missing,
;;; the corresponding default is used.  The continuation must be
;;; able to accept exactly as many arguments as ther is in the default list.

(define (with-format-parameters params defaults cont)
  (apply
   cont
   (let loop ((p params) (d defaults))
     (cond (d
	    (cons (if p 
		      (if (car p)
			  (car p)
			  (car d))
		      (car d))
		  (loop (if p (cdr p) p) (cdr d))))
	   (p
	    (format-error "Too many parameters"))
	   (else '())))))

;;; Gets the next character from the current control string.  It is an
;;; error if there is none.  Leave *format-index* pointing to the
;;; character returned.

(define (nextchar)
  (let ((new-index (1+ *format-index*)))
    (if (< new-index *format-length*)
	(begin (set! *format-index* new-index)
	       (string-ref *format-control-string* *format-index*))
	(format-error "Syntax error"))))

'$split-file

;;; Returns the current character, i.e. the one pointed to by *format-index*.

(define (format-peek)
  (string-ref *format-control-string* *format-index*))

;;; Returns the index of the first occurrence of the specified character
;;; between indices START (inclusive) and END (exclusive) in the control
;;; string.

(define (format-find-char char start end)
  (substring-find-next-char *format-control-string* start end char))

(define (add-formatter! character formatter)
  (vector-set! *format-dispatch-table* (char-code (char-upcase character)) formatter)
  (vector-set! *format-dispatch-table* (char-code (char-downcase character)) formatter))

;;; ERRORS

;;; Since errors may occur while an indirect control string is being
;;; processed, i.e. by ~? or ~{~:}, some sort of backtrace is necessary
;;; in order to indicate the location in the control string where the
;;; error was detected.  To this end, errors detected by format are
;;; signalled by throwing a list of the form ((control-string args))
;;; to FORMAT-ERROR.  This throw will be caught at each level
;;; of indirection, and the list of error messages re-thrown with an
;;; additional message indicating that indirection was present CONSed
;;; onto it.  Ultimately, the last throw will be caught by the top level
;;; FORMAT function, which will then signal an error to the common-lisp error
;;; system in such a way that all the errror messages will be displayed
;;; in reverse order.

(define (format-error complaint . args)
  (*format-error*
   (list (list "~S~%~V@T^~%;;; Format Error: ~?"
	       *format-control-string*
	       (1+ *format-index*)
	       complaint
	       args))))


;;; CONTROL STRING PARSING 

;;; The current control string is kept in *format-control-string*. 
;;; The variable *format-index* is the position of the last character
;;; processed, indexing from zero.  The variable *format-length* is the
;;; length of the control string, which is one greater than the maximum
;;; value of *format-index*.  

;;; Attempts to parse a parameter, starting at the current index.
;;; Returns the value of the parameter, or NIL if none is found. 
;;; On exit, *format-index* points to the first character which is
;;; not a part of the recognized parameter.

(define (parse-number number)
  (nextchar)
  (if (not (char-numeric? (format-peek)))
      number
      (parse-number (+ (* 10 number) (char->digit (format-peek))))))

'$split-file

(define (format-get-parameter)
  (let ((c (format-peek)))
    (cond ((char=? c #\#)
	   (let ((a (length *format-arguments*)))
	     (nextchar)
	     a))
	  ((char-ci=? c #\V)
	   (let ((a (pop-format-arg)))
	     (nextchar)
	     a))
	  ((char=? c #\')
	   (let ((a (nextchar)))
	     (nextchar)
	     a))
	  ((char-numeric? c)
	   (parse-number (char->digit (format-peek))))
	  ((char=? c #\-)
	   (nextchar)
	   (let ((next-c (format-peek)))
	     (if (char-numeric? next-c)
		 (- (parse-number (char->digit next-c)))
		 (begin (set! *format-index* (- *format-index* 1))
			'()))))
	  ((char=? c #\+)
	   (nextchar)
	   (let ((next-c (format-peek)))
	     (if (char-numeric? next-c)
		 (parse-number (char->digit next-c))
		 (begin (set! *format-index* (- *format-index* 1))
			'()))))
	  (else '()))))

;;; Parses a format directive, including flags and parameters.  On entry,
;;; *format-index* should point to the "~" preceding the command.  On
;;; exit, *format-index* points to the command character itself.
;;; Returns the list of parameters, the ":" flag, the "@" flag, and the
;;; command character as multiple values.  Explicitly defaulted parameters
;;; appear in the list of parameters as NIL.  Omitted parameters are simply 
;;; not included in the list at all.

;;; Using setqs below since can't guarantee ordering of eval to VALUES,
;;;  and originally this code used that order to bump char index properly -las

(define parameter-characters
  (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
	    #\, #\# #\V #\v #\' #\+ #\-))
  
'$split-file

(define (parse-format-operation)
  (let ((ch (nextchar))
	(parms)
	(colon)
	(atsign)
	(command))
    (set! parms (if (char-set-member? parameter-characters ch)
		    (let loop ((current-item (format-get-parameter)))
		      (if (char=? (format-peek) #\,)
			  (begin
			    (nextchar)
			    (cons current-item
				  (loop (format-get-parameter))))
			  (list current-item)))
		    '()))
    (set! colon (if (char=? (format-peek) #\:) (nextchar) '()))
    (set! atsign (if (char=? (format-peek) #\@) (nextchar) '()))
    (set! command (format-peek))
    (prim-values parms colon atsign command)))

;;; Starting at the current value of *format-index*, finds the first
;;; occurrence of one of the specified directives. Embedded constructs,
;;; i.e. those inside ~(~), ~[~], ~{~}, or ~<~>, are ignored.  And error is
;;; signalled if no satisfactory command is found.  Otherwise, the
;;; following are returned as multiple values:
;;;
;;;     The value of *format-index* at the start of the search
;;;     The index of the "~" character preceding the command
;;;     The parameter list of the command
;;;     The ":" flag
;;;     The "@" flag
;;;     The command character
;;;
;;; Implementation note:  The present implementation is not particulary
;;; careful with storage allocation.  It would be a good idea to have
;;; a separate function for skipping embedded constructs which did not
;;; bother to cons parameter lists and then throw them away.

(define close-curley-brace (char-set #\}))
(define close-angle-bracket (char-set #\>))
(define close-paren (char-set #\)))
(define close-square-bracket (char-set #\]))
(define closing-delimiters (char-set #\} #\> #\) #\]))
(define semicolon (char-set #\;))

'$split-file

(define (format-find-command command-char-set)
  (let ((start *format-index*))
    (let loop ((place start)
	       (tilde (format-find-char #\~ start *format-length*)))
      (if (not tilde)
	  (format-error "Expecting one of ~{~~~C ~}" (char-set-members command-char-set))
	  (begin
	    (set! *format-index* tilde)
	    (prim-with-values
	     (lambda () (parse-format-operation))
	     (lambda (parms colon atsign command)
	       (cond ((char-set-member? command-char-set command)
		      (prim-values start tilde parms colon atsign command))
		     ((char=? #\{ command)
		      (nextchar)
		      (format-find-command close-curley-brace)
		      (loop *format-index* 
			    (format-find-char #\~ place *format-length*)))
		     ((char=? #\< command)
		      (nextchar)
		      (format-find-command close-angle-braket)
		      (loop *format-index*
			    (format-find-char #\~ place *format-length*)))
		     ((char=? #\( command)
		      (nextchar)
		      (format-find-command close-paren)
		      (loop *format-index*
			    (format-find-char #\~ place *format-length*)))
		     ((char=? #\[ command)
		      (nextchar)
		      (format-find-command close-square-bracket)
		      (loop *format-index*
			    (format-find-char #\~ place *format-length*)))
		     ((char-set-member? closing-delimiters command)
		      (format-error "No matching bracket"))
		     (else (loop *format-index*
				 (format-find-char #\~ place *format-length*)))))))))))

'$split-file

;;; This is the FORMAT top-level function.

(define (cl-format destination control-string . format-arguments)
  (fluid-let ((*format-original-arguments* format-arguments)
	      (*format-arguments* format-arguments)
	      (*format-control-string* (coerce-to-simple-string control-string)))
    (cond ((null? destination)
	   (fluid-let ((*format-saved-output* *standard-output*))
	     (format-stringify-output
	      (lambda ()
		(setup-format-error-and-go)))))
	  ((and ((symbol-function 'stringp) destination) 
		((symbol-function 'array-has-fill-pointer-p) destination))
	   (fluid-let ((*format-saved-output* *standard-output*)
		       (*standard-output* (string-open destination 
						       (keyword-intern "OUTPUT")
						       0
						       0)))
	     (dynamic-wind (lambda () '())
			   (lambda () (setup-format-error-and-go))
			   (lambda () ((symbol-function 'close) *standard-output*)))))
	  ((eq? destination #t)
	   (fluid-let ((*format-saved-output* *standard-output*))
	     (setup-format-error-and-go)))
	  ((eq? (object-type destination) 'cl-stream)
	   (fluid-let ((*format-saved-output* *standard-output*)
		       (*standard-output* destination))
	     (setup-format-error-and-go)))
	  (else (write-string ";;; Error: " *error-output*)
		((symbol-function 'prin1) destination *error-output*)
		(write-string " is invalid destination for FORMAT" *error-output*)
		(error "" *the-non-printing-object*)))))

'$split-file

(define (setup-format-error-and-go)
  (let ((errorp 
	 (call-with-current-continuation
	  (lambda (cont)
	    (fluid-let ((*format-error* cont)
			(*format-escape* cont)
			(*format-colon-escape* cont))
	      (sub-format 0 (string-length *format-control-string*))
	      (force-output)
	      #f)))))
    (if errorp
	(fluid-let ((*standard-output* *format-saved-output*))
	  (cl-format *error-output* "~:{~%~@?~}" (reverse errorp))
	  (error "" *the-non-printing-object*)))))

;;; This function does the real work of format.  The segment of the control
;;; string between indiced START (inclusive) and END (exclusive) is processed
;;; as follows: Text not part of a directive is output without further
;;; processing.  Directives are parsed along with their parameters and flags,
;;; and the appropriate handlers invoked with the arguments COLON, ATSIGN, and
;;; PARMS. 

(define (sub-format start end)
  (fluid-let ((*format-index* start)
	      (*format-length* end))
    (define (sub-format-loop place tilde)
      (if (not tilde)
	  (internal-write-string-start-end *format-control-string*
					   place end)
	  (begin
	    (if (> tilde place)
		(internal-write-string-start-end *format-control-string*
						 place tilde))
    	    (set! *format-index* tilde)
	    (prim-with-values
	     (lambda () (parse-format-operation))
	     (lambda (parms colon atsign command)
	       ((vector-ref *format-dispatch-table* (char-code command))
	      	colon
	      	atsign
	      	parms
	      	command)))
	    (set! *format-index* (1+ *format-index*))
	    (if (< *format-index* end)
		(sub-format-loop *format-index* (format-find-char #\~ *format-index* end))))))
    (sub-format-loop start (format-find-char #\~ start end))))

'$split-file  ; This was the first split marker when the dataflow was taking forever

;;; Conditional case conversion  ~( ... ~)

(define capitalization-escapes (char-set #\) #\^))

(define string-capitalize! (make-primitive-procedure 'cl-string-capitalize!))

(define (format-capitalization colon atsign parms command)
  (if parms
      (format-error "No parameters allowed to ~~(")
      (begin (nextchar)
	     (let loop ()
	       (prim-with-values
		(lambda () (format-find-command capitalization-escapes))
		(lambda (prev tilde end-parms end-colon end-atsign cmd)
		  (if (and (char=? cmd #\))
			   (or end-parms end-colon end-atsign))
		      (format-error "Flags or parameters not allowed")
		      (let ((string (format-stringify-output (lambda () (sub-format prev tilde)))))
		      	(internal-write-string
		       	 (cond ((and atsign colon)
			      	(string-upcase! string)
				string)
			       (colon
			      	(string-capitalize! string)
				string)
			       (atsign
			      	(let ((strlen (string-length string)))
				  ;; Capitalize the first word only
				  (string-downcase! string)
				  (if (zero? strlen)
				      string
				      (let inner-loop ((i 0))
					(if (or (<= strlen i) (char-alphabetic? (string-ref string i)))
					    (begin
					      (string-set! string i (char-upcase (string-ref string i)))
					      ;; Only capitalize first word of first parsed segment
					      (set! atsign '())
					      string)
					    (inner-loop (1+ i)))))))
			       (else (string-downcase! string)
				     string)))))
		  (if (char=? cmd #\))
		      'done
		      (begin (format-escape end-colon end-atsign end-parms #f)
			     (nextchar)
			     (loop)))))))))

(add-formatter! #\( format-capitalization)

;;; Up and Out (Escape)  ~^

(define (format-escape colon atsign parms command)
  (if atsign
      (format-error "FORMAT command ~~~:[~;:~]@^ is undefined" colon)
      (let ((l (length parms)))
	(if (cond ((= l 0)
		   (not *format-arguments*))
		  ((= l 1)
		   (zero? (first parms)))
		  ((= l 2)
		   (equal (first parms) (second parms)))
		  ((= l 3)
		   (let ((x (second parms)))
		     (cond ((integer? x)
			    (<= (first parms)
				x
				(third parms)))
			   ((char? x)
			    (char<=? (first parms)
				     x
				     (third parms)))
			   (else #f))))
		  (else
		   (format-error "Too many parameters")))
	    ((if colon *format-colon-escape* *format-escape*) #f)))))

(add-formatter! #\^ format-escape)

'$split-file

;;; Conditional expression  ~[ ... ]

;;; ~[ 

(define condition-escapes (char-set #\; #\]))

(define (format-untagged-condition)
  (let ((test (pop-format-arg)))
    (if (not (integer? test))
      	(format-error "Argument to ~~[ must be integer - ~S" test)
	(begin (nextchar)
	       (let loop ((count 0))
		 (if (= count test)
		     (prim-with-values
		      (lambda () (format-find-command condition-escapes))
		      (lambda (prev tilde parms colon atsign cmd)
			(cond (atsign
			       (format-error "Atsign flag not allowed"))
			      (parms
			       (format-error "No parameters allowed"))
			      (else
			       (sub-format prev tilde)
			       (if (not (char=? cmd #\]))
				   (format-find-command close-square-bracket))))))
		     (prim-with-values
		      (lambda () (format-find-command condition-escapes))
		      (lambda (prev tilde parms colon atsign cmd)
			(cond (atsign
			       (format-error "Atsign flag not allowed"))
			      (parms
			       (format-error "Parameters not allowed"))
			      ((char=? cmd #\]) 'return)
			      (colon
			       (nextchar)
			       (prim-with-values
				(lambda () (format-find-command condition-escapes))
				(lambda (prev tilde parms colon atsign cmd)
				  (sub-format prev tilde)
				  (if (not (char=? cmd #\]))
				      (format-find-command close-square-bracket)
				      'return))))
			      (else (nextchar)
				    (loop (1+ count))))))))))))

;;; ~@[

(define (format-funny-condition)
  (nextchar)
  (prim-with-values
   (lambda () (format-find-command close-square-bracket))
   (lambda (prev tilde parms colon atsign cmd)
     (cond ((or colon atsign parms)
       	    (format-error "Flags or arguments not allowed"))
	   (*format-arguments*
       	    (if (car *format-arguments*)
	      	(sub-format prev tilde)
		(let ((result (car *format-arguments*)))
		 (set! *format-arguments* (cdr *format-arguments*))
		  result)))
	   (else
       	    (format-error "Missing argument"))))))

;;; ~:[ 

(define (format-boolean-condition)
  (nextchar)
  (prim-with-values
   (lambda () (format-find-command semicolon))
   (lambda (prev tilde parms colon atsign cmd)
     (if (or parms colon atsign)
       	 (format-error "Flags or parameters not allowed")
       	 (begin (nextchar)			  
   	      	(if (pop-format-arg)
       		    (prim-with-values
		     (lambda () (format-find-command close-square-bracket))
		     (lambda (prev tilde parms colon atsign cmd)
		       (if (or colon atsign parms)
	  	       	   (format-error "Flags or parameters not allowed")
		       	   (sub-format prev tilde))))
       		    (begin
		     (sub-format prev tilde)
		     (format-find-command close-square-bracket))))))))

(define (format-condition colon atsign parms command)
  (if parms
      (let ((result (car parms)))
	(set! parms (cdr parms))
	(set! *format-arguments* (cons result *format-arguments*))
	(if parms
	    (format-error "Too many parameters to ~["))))
  (cond (colon
	 (if atsign
	     (format-error  "~~:@[ undefined")
	     (format-boolean-condition)))
	(atsign
	 (format-funny-condition))
	(else (format-untagged-condition))))

(add-formatter! #\[ format-condition)

'$split-file

;;; Iteration  ~{ ... ~}

(define (format-iteration colon atsign parms command)
  (with-format-parameters
   parms
   (list -1)
   (lambda (max-iter)
     (nextchar)
     (prim-with-values
      (lambda () (format-find-command close-curley-brace))
      (lambda (prev tilde end-parms end-colon end-atsign cmd)
      (cond ((or end-atsign end-parms)
	     (format-error "Illegal terminator for ~~{"))
      	    ((= prev tilde)
	     ;; Use an argument as the control string if ~{~} is empty
	     (let ((string (pop-format-arg)))
	       (if (not ((symbol-function 'stringp) string))
		   (format-error "Control string is not a string")
	    	   (format-with-control-string
	     	    (coerce-to-simple-string string)
	     	    (lambda ()
	       	      (format-do-iteration 0
					   *format-length*
				    	   max-iter
					   colon
					   atsign
					   end-colon))))))
	    (else
	     (format-do-iteration prev
				  tilde
				  max-iter
				  colon
				  atsign
				  end-colon))))))))

'$split-file

;;; The two catch tags FORMAT-ESCAPE and FORMAT-COLON-ESCAPE are needed here
;;; to correctly implement ~^ and ~:^.  The former aborts only the current
;;; iteration, but the latter aborts the entire iteration process.

(define (format-do-iteration start end max-iter colon atsign at-least-once-p)
  (call-with-current-continuation
   (lambda (cont)
     (fluid-let ((*format-colon-escape* cont)
		 (*format-escape* cont))
       (if atsign
	   (let loop ((count 0))
	     (if (or (= count max-iter)
		     (and (null? *format-arguments*)
			  (if (= count 0) (not at-least-once-p) #t)))
		 'return
		 (begin
		  (call-with-current-continuation
		   (lambda (cont)
		     (fluid-let ((*format-escape* cont))
	      	       (if colon
			   (fluid-let ((*format-original-arguments* (pop-format-arg)))
			     (fluid-let ((*format-arguments* *format-original-arguments*))
			       (if (not (list? *format-arguments*))
				   (format-error "Argument must be a list")
				   (sub-format start end))))
		  	   (sub-format start end)))))
		  (loop (1+ count)))))
	   (fluid-let ((*format-original-arguments* (pop-format-arg)))
	     (fluid-let ((*format-arguments* *format-original-arguments*))
	       (if (not (list? *format-arguments*))
		   (format-error "Argument must be a list")
		   (let loop ((count 0))
		     (if (or (= count max-iter)
			     (and (null? *format-arguments*)
				  (if (= count 0) (not at-least-once-p) #t)))
			 'return
			 (begin
			   (call-with-current-continuation
			    (lambda (cont)
			      (fluid-let ((*format-escape* cont))
				(if colon
				    (fluid-let ((*format-original-arguments* (pop-format-arg)))
				      (fluid-let ((*format-arguments* *format-original-arguments*))
					(if (not (list? *format-arguments*))
					    (format-error "Argument must be a list of lists")
					    (sub-format start end))))
				    (sub-format start end)))))
			   (loop (1+ count)))))))))))))

(add-formatter! #\{ format-iteration)

'$split-file


;;; Justification  ~< ... ~>

;;; Parses a list of clauses delimited by ~; and terminated by ~>.
;;; Recursively invoke SUB-FORMAT to process them, and return a list
;;; of the results, the length of this list, and the total number of
;;; characters in the strings composing the list.

(define justification-escapes (char-set #\; #\>))

(define (format-get-trailing-segments)
  (nextchar)
  (prim-with-values
   (lambda () (format-find-command justification-escapes))
   (lambda (prev tilde colon atsign parms cmd)
     (cond (colon
     	    (format-error "~~:; allowed only after first segment in ~~<"))
   	   ((or atsign parms)
     	    (format-error "Flags and parameters not allowed"))
	   (else
   	    (let ((str (call-with-current-continuation
			(lambda (cont)
			  (fluid-let ((*format-escape* cont))
			    (format-stringify-output (lambda () (sub-format prev tilde))))))))
     	      (if str
	 	  (if (char=? cmd #\;)
	     	      (prim-with-values
	      	       (lambda () (format-get-trailing-segments))
	      	       (lambda (segments numsegs numchars)
	      		 (prim-values (cons str segments)
		      		      (1+ numsegs)
				      (+ numchars (string-length str)))))
	     	      (prim-values (list str) 1 (string-length str)))
	 	  (prim-values '() 0 0))))))))

;;; Gets the first segment, which is treated specially.  Call 
;;; FORMAT-GET-TRAILING-SEGMENTS to get the rest.

(define (format-get-segments)
  (prim-with-values
   (lambda () (format-find-command justification-escapes))
   (lambda (prev tilde parms colon atsign cmd)
     (if atsign
     	 (format-error "Atsign flag not allowed")
	 (let ((first-seg
		(call-with-current-continuation
		 (lambda (cont)
		   (fluid-let ((*format-escape* cont))
		     (format-stringify-output
		      (lambda () (sub-format prev tilde))))))))
     	   (if first-seg
	       (if (char=? cmd #\;)
	     	   (prim-with-values
	      	    (lambda () (format-get-trailing-segments))
	      	    (lambda (segments numsegs numchars)
	      	      (if colon
		  	  (prim-values first-seg parms segments numsegs numchars)
		  	  (prim-values #f #f (cons first-seg segments) (1+ numsegs)
			  	       (+ (string-length first-seg) numchars)))))
		   (prim-values #f #f (list first-seg) 1 (string-length first-seg)))
	       (begin (format-find-command close-angle-bracket)
		      (prim-values #f #f #f 0 0))))))))

'$split-file

;;; Given the total number of SPACES needed for padding, and the number
;;; of padding segments needed (PADDINGS), returns a list of such segments.
;;; We try to allocate the spaces equally to each segment.  When this is
;;; not possible, we allocate the left-over spaces big end first.

(define (make-pad-segs spaces paddings)
  (if (zero? paddings)
      '()
      (let ((seg (ceiling (/ spaces paddings))))
	(cons seg
	      (make-pad-segs (- spaces seg) (-1+ paddings))))))

;;; Determine the actual width to be used for a field requiring WIDTH
;;; characters according to the following rule:  If WIDTH is less than or
;;; equal to MINCOL, use WIDTH as the actual width.  Otherwise, round up 
;;; to MINCOL + k * COLINC for the smallest possible positive integer k.

(define (format-round-columns width mincol colinc)
  (if (> width mincol)
      (let ((q+r (integer-divide (- width mincol) colinc)))
	(let ((quotient (car q+r))
	      (remainder (cdr q+r)))
       	  (+ mincol (* quotient colinc) (if (zero? remainder) 0 colinc))))
      mincol))

'$split-file

(define (format-justification colon atsign parms command)
  (with-format-parameters
   parms
   (list 0 1 0 #\space)
   (lambda (mincol colinc minpad padchar)
     (cond ((or (not (integer? mincol)) (negative? mincol))
	    (format-error "Mincol must be a non-negative integer - ~S" mincol))
     	   ((or (not (integer? colinc)) (not (positive? colinc)))
	    (format-error "Colinc must be a positive integer - ~S" colinc))
     	   ((or (not (integer? minpad)) (negative? minpad))
	    (format-error "Minpad must be a non-negative integer - ~S" minpad))
     	   ((not (char? padchar))
	    (format-error "Padchar must be a character - ~S" padchar))
           (else
	    (nextchar)
	    (prim-with-values
	     (lambda () (format-get-segments))
	     (lambda (special-arg special-parms segments numsegs numchars)
	       (if segments
		   (let* ((padsegs (+ (if (or colon 
					      (and (= numsegs 1)
						   (not atsign)))
					  1
					  0)
				      (-1+ numsegs)
				      (if atsign 1 0)))
			  (width (format-round-columns
				  (+ numchars (* minpad padsegs))
				  mincol colinc))
			  (spaces (append (if (or colon 
						  (and (= numsegs 1)
						       (not atsign)))
					      '()
					      '(0))
					  (make-pad-segs (- width numchars) padsegs)
					  (if atsign '() '(0)))))
		     (if special-arg
			 (with-format-parameters
			  special-parms
			  (list 0 80)
			  (lambda (spare linel)
			    (let ((pos (or (charpos *standard-output*) 0)))
			      (if (> (+ pos width spare) linel)
				  (internal-write-string special-arg))))))
		     (let loop ((segs segments)
		       		(spcs spaces))
		       (if (null? segs)
			   (n-write-char (car spcs) padchar)
			   (begin (n-write-char (car spcs) padchar)
				  (internal-write-string (car segs))
				  (loop (cdr segs) (cdr spcs))))))))))))))

;;; Optimized for writing 80 or fewer spaces

(define n-write-char
  (let ((blank-string (make-string 80 #\space)))
    (lambda (n char)
      (if (and (<= n 80)
	       (eq? char #\space))
	  (internal-write-string-start-end blank-string 0 n)
	  (let loop ((n n))
	    (if (= n 0)
		#f
		(begin 
		  (internal-write-char char)
		  (loop (-1+ n)))))))))

(add-formatter! #\< format-justification)

;;; Newline  ~%

(define (format-terpri colon atsign parms command)
  (if (or colon atsign)
      (format-error "Flags not allowed")
      (with-format-parameters
       parms
       (list 1)
       (letrec ((loop 
		 (named-lambda (loop repeat-count)
		   (if (> repeat-count 0)
		       (begin (internal-write-char #\newline)
			      (loop (-1+ repeat-count)))))))
	 loop))))

(add-formatter! #\% format-terpri)

'$split-file

;;; Fresh-line  ~&

(define (format-freshline colon atsign parms command)
  (if (or colon atsign)
      (format-error "Flags not allowed")
      (with-format-parameters
       parms
       (list 1)
       (lambda (repeat-count)
     	 (if (not (zero? repeat-count))
	     ((symbol-function 'fresh-line)))
     	 (let loop ((n (-1+ repeat-count)))
       	   (if (positive? n)
	       (begin (internal-write-char #\newline)
		      (loop (-1+ n)))))))))

(add-formatter! #\& format-freshline)

;;; Page  ~|

(define (format-page colon atsign parms command)
  (if (or colon atsign)
      (format-error "Flags not allowed")
      (with-format-parameters
       parms
       (list 1)
       (letrec ((loop 
		 (named-lambda (loop repeat-count)
		   (if (> repeat-count 0)
		       (begin (internal-write-char #\page)
			      (loop (-1+ repeat-count)))))))
	 loop))))
  
(add-formatter! #\| format-page)

;;; Print a tilde  ~~

(define (format-tilde colon atsign parms command)
  (if (or colon atsign)
      (format-error "Flags not allowed")
      (with-format-parameters
       parms
       (list 1)
       (letrec ((loop 
		 (named-lambda (loop repeat-count)
		   (if (> repeat-count 0)
		       (begin (internal-write-char #\~)
			      (loop (-1+ repeat-count)))))))
	 loop))))

(add-formatter! #\~ format-tilde)

;;; Continue control string on next line  ~<newline>

(define (format-eat-whitespace)
  (cond ((= *format-index* (-1+ *format-length*))
	 'return)
	((char-whitespace? (format-peek))
	 (nextchar)
	 (format-eat-whitespace))
	(else (set! *format-index* (-1+ *format-index*)))))

(define (format-newline colon atsign parms command)
  (if parms
      (format-error "Parameters not allowed")
      (cond (colon
	     (if atsign (format-error "~:@<newline> is undefined")))
      	    (atsign
	     (internal-write-char #\newline)
	     (format-eat-whitespace))
	    (else (format-eat-whitespace)))))

(add-formatter! #\newline format-newline)

;;; Pluralize word  ~P

(define (format-plural colon atsign parms command)
  (if parms
      (format-error "Parameters not allowed")
      (begin
       (if colon
    	   ;; Back up one argument first
    	   (let ((cdrs (- (length *format-original-arguments*)
		   	  (length *format-arguments*)
		   	  1)))
      	     (if (negative? cdrs)
	  	 (format-error  "No previous argument")
	  	 (set! *format-arguments*
		       (list-tail *format-original-arguments* cdrs)))))
       (if (= (pop-format-arg) 1)
      	   (internal-write-string (if atsign "y" ""))
      	   (internal-write-string (if atsign "ies" "s"))))))

(add-formatter! #\P format-plural)

;;; Skip arguments  (relative goto)  ~*

(define (format-skip-arguments colon atsign parms command)
  (with-format-parameters
   parms
   (list '())
   (lambda (count)
     (cond ((and atsign colon)
	    (format-error "~:@* is undefined"))
	   (atsign
	    (if (null? count)
		(set! count 0))
	    (if (or (negative? count)
		    (> count (length *format-original-arguments*)))
		(format-error "Illegal to go to non-existant argument")
	    	(set! *format-arguments*
		      (list-tail *format-original-arguments* count))))
	   (colon
	    (if (null? count)
		(set! count 1))
	    (let ((cdrs (- (length *format-original-arguments*)
			   (length *format-arguments*)
			   count)))
	      (if (negative? cdrs)
		  (format-error "Skip to nonexistant argument")
		  (set! *format-arguments*
			(list-tail *format-original-arguments* cdrs)))))
	   (else
	    (if (null? count)
		(set! count 1))
	    (if (> count (length *format-arguments*))
		(format-error "Skip to nonexistant argument")
		(set! *format-arguments*
		      (list-tail *format-arguments* count))))))))

(add-formatter! #\* format-skip-arguments)

'$split-file

;;; Indirection  ~?

(define (format-indirection colon atsign parms command)
  (if (or colon parms)
      (format-error "Colon flag or parameters not allowed")
      (let ((string (pop-format-arg)))
    	(if (not ((symbol-function 'stringp) string))
	    (format-error "Indirected control string is not a string")
    	    (format-with-control-string
     	     (coerce-to-simple-string string)
     	     (lambda ()
       	       (if atsign
	   	   (sub-format 0 *format-length*)
		   (fluid-let ((*format-original-arguments* (pop-format-arg)))
		     (fluid-let ((*format-arguments* *format-original-arguments*))
	     	       (if (not (list? *format-arguments*))
			   (format-error "Argument must be a list")
	     	     	   (sub-format 0 *format-length*)))))))))))

(add-formatter! #\? format-indirection)

;;; Tabulation  ~T

(define charpos (make-primitive-procedure 'cl-charpos))

(define (format-tab colon atsign parms command)
  (with-format-parameters
   parms
   (list 1 1)
   (lambda (colnum colinc)
     (cond (colon (format-error "~:T is undefined"))
	   (atsign
	    (n-write-char colnum #\space)
	    (let ((position (charpos)))
	      (if position
		  (n-write-char (modulo (- colinc (remainder position colinc)) colinc) #\space))))
	   (else
	    (let ((position (charpos)))
	      (cond ((< position colnum)
		     (n-write-char (- colnum position) #\space))
		    ((zero? colinc) '())
		    (else
		     (n-write-char (- colinc (remainder (- position colnum) colinc)) #\space)))))))))

(add-formatter! #\T format-tab)

;;; Ascii  ~A

(define (format-princ colon atsign parms command)
  (let ((arg (pop-format-arg)))
    (if (null? parms)
	(if arg
	    ((symbol-function 'princ) arg)
	    (internal-write-string (if colon "()" "NIL")))
	(with-format-parameters
	 parms
	 (list 0 1 0 #\space)
	 (lambda (mincol colinc minpad padchar)
	   (format-write-field (if arg
				   ((symbol-function 'princ-to-string) arg)
				   (if colon "()" "NIL"))
			       mincol colinc minpad padchar atsign))))))

(add-formatter! #\A format-princ)

;;; S-expression  ~S
	    
(define (format-prin1 colon atsign parms command)
  (let ((arg (pop-format-arg)))
    (if (null? parms)
	(if arg
	    ((symbol-function 'prin1) arg)
	    (internal-write-string (if colon "()" "NIL")))
	(with-format-parameters
	 parms
	 (list 0 1 0 #\space)
	 (lambda (mincol colinc minpad padchar)
	   (format-write-field (if arg
				   ((symbol-function 'prin1-to-string) arg)
				   (if colon "()" "NIL"))
			       mincol colinc minpad padchar atsign))))))

(add-formatter! #\S format-prin1)

;;; Character  ~C

(define (format-print-character colon atsign parms command)
  (with-format-parameters
   parms
   '()
   (lambda ()
    (let ((char (pop-format-arg)))
      (cond ((not (char? char))
	     (format-error "Argument must be a character"))
      	    ((and atsign (not colon))
	     ((symbol-function 'prin1) char))
	    (else (format-print-named-character char)))))))

(define (format-print-named-character char)
  (let ((bits (char-bits char)))
    (if (not (zero? bits))
	(begin
	  (if (control-bit? bits)
	      (internal-write-string "Control-"))
	  (if (meta-bit? bits)
	      (internal-write-string "Meta-"))
	  (if (super-bit? bits)
	      (internal-write-string "Super-"))
	  (if (hyper-bit? bits)
	      (internal-write-string "Hyper-"))
	  (let ((prim-char (code->char (char-code char))))
	    (if (char-lower-case? prim-char)
		(internal-write-char #\\))
	    (internal-write-string (char->name prim-char))))
	(internal-write-string (char->name char)))))

(add-formatter! #\C format-print-character)

'$split-file

;;; NUMERIC PRINTING

;;; Insert commas after every third digit, scanning from right to left.

(define (format-add-commas string commachar)
  (let ((length (string-length string)))
    (let ((new-length (+ length (quotient (-1+ length) 3))))
      (let ((new-string (make-string new-length commachar)))
	(let loop ((count 0)
		   (new-count 0))
	  (if (= count length)
	      new-string
	      (begin (string-set! new-string (- new-length new-count 1)
				  (string-ref string (- length count 1)))
		     (if (zero? (modulo (1+ count) 3))
			 (loop (1+ count) (+ new-count 2))
			 (loop (1+ count) (1+ new-count))))))))))

;;; Output a string in a field at MINCOL wide, padding with PADCHAR.
;;; Pads on the left if PADLEFT is true, else on the right.  If the
;;; length of the string plus the minimum permissible padding, MINPAD,
;;; is greater than MINCOL, the actual field size is rounded up to
;;; MINCOL + k * COLINC for the smallest possible positive integer k.

(define (format-write-field string mincol colinc minpad padchar padleft)
  (cond ((or (not (integer? mincol)) (negative? mincol))
    	 (format-error "Mincol must be a non-negative integer - ~S" mincol))
  	((or (not (integer? colinc)) (not (positive? colinc)))
    	 (format-error "Colinc must be a positive integer - ~S" colinc))
  	((or (not (integer? minpad)) (negative? minpad))
    	 (format-error "Minpad must be a non-negative integer - ~S" minpad))
  	((not (char? padchar))
	 (format-error "Padchar must be a character - ~S" padchar))
	(else
	 (let ((strlen (string-length string)))
	   (let ((width (format-round-columns (+ strlen minpad) mincol colinc)))
    	     (if padleft
	   	 (begin
		  (n-write-char (- width strlen) padchar)
		  (internal-write-string string))
		 (begin
		  (internal-write-string string)
	   	  (n-write-char (- width strlen) padchar))))))))

;;; This functions does most of the work for the numeric printing
;;; directives.  The parameters are interpreted as defined for ~D.

(define (format-print-number number radix print-commas-p print-sign-p parms)
  (with-format-parameters
   parms
   (list 0 #\space #\,)
   (lambda (mincol padchar commachar)
     (fluid-let ((*print-base* radix))
       (let ((text ((symbol-function 'princ-to-string) number)))
       	 (if (integer? number)
	     (format-write-field
	      (if (and (positive? number) print-sign-p)
		  (if print-commas-p
		      (string-append "+" (format-add-commas text commachar))
		      (string-append "+" text))
		  (if print-commas-p
		      (format-add-commas text commachar)
		      text))
	      mincol 1 0 padchar t)	;colinc = 1, minpad = 0, padleft = t
	     (internal-write-string text)))))))

;;; Print a cardinal number in English

(define cardinal-ones
  #(#f "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(define cardinal-tens
  #(#f #f "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"))

(define cardinal-teens
  #("ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"))

(define truncate (make-primitive-procedure 'generic-truncate))

'$split-file

(define (format-print-small-cardinal n)
  (prim-with-values
   (lambda () (truncate n 100))
   (lambda (hundreds rem)
     (if (positive? hundreds)
	 (begin
	  (internal-write-string (vector-ref cardinal-ones hundreds))
	  (internal-write-string " hundred")
	  (if (positive? rem) (internal-write-char #\space))))
     (if (positive? rem)
	 (prim-with-values
	  (lambda () (truncate rem 10))
	  (lambda (tens ones)
	    (cond ((< 1 tens)
	      	   (internal-write-string (vector-ref cardinal-tens tens))
	      	   (if (positive? ones)
		       (begin (internal-write-char #\-)
			      (internal-write-string (vector-ref cardinal-ones ones)))))
	     	  ((= tens 1)
	      	   (internal-write-string (vector-ref cardinal-teens ones)))
	     	  ((positive? ones)
	      	   (internal-write-string (vector-ref cardinal-ones ones))))))))))

(define cardinal-periods
  #("" " thousand" " million" " billion" " trillion" " quadrillion"
       " sextillion" " septillion" " octillion" " nonillion" " decillion"))

(define (format-print-cardinal n)
  (cond ((negative? n)
	 (internal-write-string "negative ")
	 (format-print-cardinal-aux (- n) 0 n))
	((zero? n)
	 (internal-write-string "zero"))
	(else (format-print-cardinal-aux n 0 n))))

(define (format-print-cardinal-aux n period err)
  (prim-with-values
   (lambda () (truncate n 1000))
   (lambda (beyond here)
    (if (> period 10)
	(format-error "Number too large to print in English: ~:D" err))
    (if (not (zero? beyond))
	(format-print-cardinal-aux beyond (1+ period) err))
    (if (not (zero? here))
	(if (not (zero? beyond)) (internal-write-char #\space)))
    (format-print-small-cardinal here)
    (internal-write-string (vector-ref cardinal-periods period)))))

;;; Print an ordinal number in English

(define ordinal-ones
  #(#f "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth" "ninth"))

(define ordinal-tens 
  #(#f "tenth" "twentieth" "thirtieth" "fortieth" "fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth"))

(define (format-print-ordinal n)
  (if (negative? n)
      (internal-write-string "negative "))
  (let ((number (abs n)))
    (prim-with-values
     (lambda () (truncate number 100))
     (lambda (top bot) 
     (if (not (zero? top))
	 (format-print-cardinal (- number bot)))
     (if (and (positive? top) (positive? bot))
	 (internal-write-char #\space))
     (prim-with-values
      (lambda () (truncate bot 10))
      (lambda (tens ones) 
      	(cond ((= bot 12) (internal-write-string "twelfth"))
	      ((= tens 1)
	       (internal-write-string (vector-ref cardinal-teens ones))
	       (internal-write-string "th"))
	      ((and (zero? tens) (positive? ones))
	       (internal-write-string (vector-ref ordinal-ones ones)))
	      ((and (zero? ones) (positive? tens))
	       (internal-write-string (vector-ref ordinal-tens tens)))
	      ((positive? bot)
	       (internal-write-string (vector-ref cardinal-tens tens))
	       (internal-write-char #\-)
	       (internal-write-string (vector-ref ordinal-ones ones)))
	      ((positive? number) (internal-write-string "th"))
	      (else (internal-write-string "zeroth")))))))))

'$split-file

;;; Print Roman numerals

(define (format-print-old-roman n)
  (if (not (< 0 n 5000))
      (format-error "Number out of range to print in old Roman numerals: ~:D" n)
      (let loop ((char-list '(#\D #\C #\L #\X #\V #\I #f))
		 (val-list '(500 100 50 10 5 1 #f))
		 (cur-char #\M)
		 (cur-val 1000)
		 (start n))
	(if (zero? start)
	    '()
	    (loop (cdr char-list)
		  (cdr val-list)
		  (car char-list)
		  (car val-list)
		  (let inner-loop ((i start))
		    (if (< i cur-val)
			i
			(begin (internal-write-char cur-char)
			       (inner-loop (- i cur-val))))))))))

(define (format-print-roman n)
  (if (not (< 0 n 4000))
      (format-error "Number out of range to print in Roman numerals: ~:D" n)
      (let loop ((char-list '(#\D #\C #\L #\X #\V #\I #f))
	    	 (val-list '(500 100 50 10 5 1 #f))
	    	 (sub-chars '(#\C #\X #\X #\I #\I #f #f))
	    	 (sub-val '(100 10 10 1 1 0 #f))
	    	 (cur-char #\M)
	    	 (cur-val 1000)
	    	 (cur-sub-char #\C)
	    	 (cur-sub-val 100)
	    	 (start n))
	(if (zero? start)
	    '()
	    (loop (cdr char-list)
		  (cdr val-list)
		  (cdr sub-chars)
		  (cdr sub-val)
		  (car char-list)
		  (car val-list)
		  (car sub-chars)
		  (car sub-val)
		  (let inner-loop ((i start))
		    (if (< i cur-val)
		     	(if (<= (- cur-val cur-sub-val) i)
			    (begin (internal-write-char cur-sub-char)
			       	   (internal-write-char cur-char)
			       	   (- i (- cur-val cur-sub-val)))
			    i)
			(begin
			 (internal-write-char cur-char)
			 (inner-loop (- i cur-val))))))))))
'$split-file


;;; Decimal  ~D

(define (format-print-decimal colon atsign parms command)
  (format-print-number (pop-format-arg) 10 colon atsign parms))

(add-formatter! #\D format-print-decimal)

;;; Binary  ~B

(define (format-print-binary colon atsign parms command)
  (format-print-number (pop-format-arg) 2 colon atsign parms))

(add-formatter! #\B format-print-binary)

;;; Octal  ~O

(define (format-print-octal colon atsign parms command)
  (format-print-number (pop-format-arg) 8 colon atsign parms))

(add-formatter! #\O format-print-octal)

;;; Hexadecimal  ~X

(define (format-print-hexadecimal colon atsign parms command)
  (format-print-number (pop-format-arg) 16 colon atsign parms))

(add-formatter! #\X format-print-hexadecimal)

;;; Radix  ~R

(define (format-print-radix colon atsign parms command)
  (let ((number (pop-format-arg)))
    (if parms
	(format-print-number number (or (car parms) 10) colon atsign (cdr parms))
	(if atsign
	    (if colon
		(format-print-old-roman number)
		(format-print-roman number))
	    (if colon
		(format-print-ordinal number)
		(format-print-cardinal number))))))

(add-formatter! #\R format-print-radix)

;;; FLOATING-POINT NUMBERS

;;; Fixed-format floating point  ~F

(define (format-fixed colon atsign parms command)
  (if colon
      (format-error "Colon flag not allowed")
      (with-format-parameters
       parms
       (list #f #f #f #f #\space)
       (lambda (w d k ovf pad)
     	 (let ((number (pop-format-arg)))
       	   (cond ((flonum? number)
	       	  (format-fixed-aux number w d k ovf pad atsign))
	       	 ((rational? number)
	       	  (format-fixed-aux
		   (coerce-to-float number)
		   w d k ovf pad atsign))
		 (else
	       	  (fluid-let ((*print-base* 10))
		    (format-write-field
		     ((symbol-function 'princ-to-string) number) (or w 0) 1 0 #\space #t)))))))))

(define format-fixed-flonum-to-string (make-primitive-procedure 'cl-format-fixed-flonum-to-string))

(define (conversion-string size)
  (let ((z (stream-info *standard-output*)))
    (if (or (null? z)
	    (< (string-length z) size))
	(begin
	  (set-stream-info! *standard-output* (make-string size))
	  (stream-info *standard-output*))
	z)))	

(define (calculate-length width digits)
  (cond (width (+ width 10))
	(digits (+ digits 10))
	(else 128)))

(define (format-fixed-aux number w d k ovf pad atsign)
  (if (not (or w d))
      ((symbol-function 'prin1) number)
      (let ((spaceleft w))
	(if (and w (or atsign (negative? number)))
	    (set! spaceleft (-1+ spaceleft)))
	(let ((str (conversion-string (calculate-length w d))))
	  (let ((len (format-fixed-flonum-to-string (abs number) str spaceleft d k)))
	    (if w (set! spaceleft (- spaceleft len)))
	    (if (and w (< spaceleft 0) ovf)
		;;field width overflow
		(n-write-char w ovf)
		(begin
		  (if w (n-write-char spaceleft pad))
		  (if (negative? number)
		      (internal-write-char #\-)
		      (if atsign (internal-write-char #\+)))
		  (internal-write-string-start-end str 0 len))))))))

'$split-file

(add-formatter! #\F format-fixed)

;;; Exponential-format floating point  ~E

(define (format-exponential colon atsign parms command)
  (if colon
      (format-error "Colon flag not allowed")
      (with-format-parameters
       parms
       (list #f #f #f 1 #f #\space #f)
       (lambda (w d e k ovf pad marker)
     	 (let ((number (pop-format-arg)))
       	   (cond ((flonum? number)
	       	  (format-exp-aux number w d e k ovf pad marker atsign))
	   	 ((rational? number)
	       	  (format-exp-aux
		   (coerce-to-float number)
		   w d e k ovf pad marker atsign))
	       	 (else (fluid-let ((*print-base* 10))
		 	 (format-write-field
		  	  ((symbol-function 'princ-to-string) number)
			  (or w 0) 1 0 #\space #t)))))))))

(define computerized-flonum-to-string 
  (make-primitive-procedure 'cl-computerized-flonum-to-string))

(define format-exponential-flonum-to-string 
  (make-primitive-procedure 'cl-format-exponential-flonum-to-string))

(define (format-exp-aux number w d e k ovf pad marker atsign)
  (if (not (or w d e))
      (let ((str (conversion-string 128)))
	(let ((len (computerized-flonum-to-string number str)))
	  (internal-write-string-start-end str 0 len)))
      (let ((str (conversion-string (calculate-length w d))))
	(if (and (number? k)
		 (number? d)
		 (or (and (> k 0) (>= k (+ d 2)))
		     (and (< k 0) (<= k (- d)))))
	    (format-error "d (number of digits) and k (scale factor) incompatible")
	    (let ((sign? (or atsign (negative? number))))
	      (let ((len (format-exponential-flonum-to-string (abs number)
							      str
							      (if (and sign? w)
								  (-1+ w)
								  w)
							      d
							      k
							      e
							      marker)))
		(if (or (negative? len)
			(and w
			     (> (+ len (if sign? 1 0)) w)))
		    (if (and ovf w)
			(n-write-char w ovf)
			(begin
			  (if sign? (internal-write-char (if (negative? number) #\- #\+)))
			  (internal-write-string-start-end str 0 len)))
		    (begin
		      (n-write-char
		       (if w
			   (- w (+ len (if sign? 1 0)))
			   0)
		       pad)
		      (if sign? (internal-write-char (if (negative? number) #\- #\+)))
		      (internal-write-string-start-end str 0 len)))))))))

(add-formatter! #\E format-exponential)

'$split-file

;;; General Floating Point -  ~G

(define (format-general-float colon atsign parms command)
  (if colon
      (format-error "Colon flag not allowed")
      (with-format-parameters
       parms
       (list #f #f #f #f #\* #\space #f)
       (lambda (w d e k ovf pad marker)
     	 (let ((number (pop-format-arg)))
       	   (cond ((flonum? number)
	       	  (format-general-aux number w d e k ovf pad marker atsign))
	       	 ((rational? number)
	       	  (format-general-aux
		   (coerce-to-float number)
		   w d e k ovf pad marker atsign))
	       	 (else (fluid-let ((*print-base* 10))
		 	 (format-write-field
		  	  ((symbol-function 'princ-to-string) number)
			  (or w 0) 1 0 #\space #t)))))))))

(define (format-general-aux number w d e k ovf pad marker atsign)
  (let* ((n (if (zero? number)
		0
		(1+ (floor (/ (log (abs number)) (log 10))))))
	 (ee (if e (+ e 2) 4))
	 (ww (if w (- w ee) #f)))
    (if (not d)
	(set! d (max (number-of-digits number) (min n 7))))
    (let ((dd (- d n)))
      (if (<= 0 dd d)
	  (begin (format-fixed-aux number ww dd #f ovf pad atsign)
		 (n-write-char ee #\space))
	  (format-exp-aux 
	   number w d e (or k 1) ovf pad marker atsign)))))

(define (number-of-digits number)
  (if (zero? number)
      1
      (let ((str (conversion-string 128)))
	(let ((len (format-fixed-flonum-to-string (abs number) str nil nil nil))) 
	  (- len
	     1 ;; One for decimal point
	     (if (char=? (string-ref str (- len 1)) #\0)
		 1 ;; One for trailing zero
		 0)
	     (if (char=? (string-ref str 0) #\0)
		 1 ;; One for leading zero
		 0))))))

(add-formatter! #\G format-general-float)

;;; Dollars floating-point format  ~$

(define (format-dollars colon atsign parms command)
  (with-format-parameters
   parms
   (list 2 1 0 #\space)
   (lambda (d n w pad)
     (let ((number (pop-format-arg)))
       (if (and (rational? number)
		(not (flonum? number)))
	   (set! number (coerce-to-float number)))
       (if (flonum? number)
	   (let ((signstr (if (negative? number) "-" (if atsign "+" "")))
		 (the-number (abs number))
		 (str (conversion-string (calculate-length d nil))))
	     (let ((signlen (string-length signstr))
		   (strlen (format-fixed-flonum-to-string the-number str nil d nil)))
	       (let ((pointplace (string-find-next-char str #\.)))
		 (if colon (internal-write-string signstr))
		 (n-write-char (- w signlen (- n pointplace) strlen) pad)
		 (if (not colon) (internal-write-string signstr))
		 (n-write-char (- n pointplace) #\0)
		 (internal-write-string (substring str 0 strlen)))))
	   (fluid-let ((*print-base* 10))
	     (format-write-field
	      ((symbol-function 'princ-to-string) number)
	      w 1 0 #\space #t)))))))

(add-formatter! #\$ format-dollars)

#f ; To avoid unassigned error from compiled set!
 
