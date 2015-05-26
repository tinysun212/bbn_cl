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
(export '(subtypep))

;;; It is assumed that a type specifier has no futures in it except perhaps
;;; at the topmost level.

(proclaim '(insert-touches nil))

;;; Type-Arg  --  Internal
;;;
;;;    Grab the n'th argument out of a type specifier, defaulting
;;; to Default if it doesn't exist.
;;;

(defun type-arg (type &optional (arg 0) (default '*))
  (let ((arg (1+ arg)))
    (if (and (consp type) (> (length type) arg))
	(elt (the list type) arg)
	default)))

;;; Type-Name  --  Internal
;;;
;;;    Return the name of a type specifier.  Either the car if the specifier
;;; is a list, or the specifier itself.
;;;

(defun type-name (type)
  (if (consp type)
      (car type)
      type))

;;; Subtypep-specialist-Tables

;;; The Subtypep-specialist tables are A lists of (name . function) where name
;;;  is a symbol which is the car of some list style type specifier.  The
;;;  specialist functions are versions of Sub-Subtypep which work for one 
;;;  particular case.  The entry for foo in the specialist-1 table handles
;;;  the case for (sub-subtypep (foo ...) xxx), and the entry for foo in the
;;;  specialist-2 table handles the case for (sub-subtypep xxx (foo ...)).
;;;
;;; The specialists in the type1 table are usually more comprehensive, because
;;;  the type1 specialists are given the first chance.

(defvar *stp-specialist-1-table* ())
(defvar *stp-specialist-2-table* ())

;;; Def-subtypep-specialist defines a specialist for handling list type
;;;  specifiers.  Name is the car of the list types to be handled, and number
;;;  distinguishes whether this is the specialist for type1 or type2.  Forms 
;;;  are the body of a function which has two args, type1 and type2.  The 
;;;  form returned by the macro, pushes the binding of the specialist function
;;;  on the appropriate a-list.  Name can be a list of names, as well.

(defmacro def-subtypep-specialist (number name &rest forms)
  (if (listp name)
      `(let ((fun (function (lambda (type1 type2) ,@forms))))
	 ,@(mapcar (function (lambda (name)
		       `(push (cons ',name fun)
			      ,(if (= number 1)
				   '*stp-specialist-1-table*
				   '*stp-specialist-2-table*))))
		   name))
      `(push (cons ',name (function (lambda (type1 type2) ,@forms)))
	     ,(if (= number 1)
		  '*stp-specialist-1-table*
		  '*stp-specialist-2-table*))))

;;; There are several specialists for lost causes.  They might as well all be
;;;  the same function.

(defun always-too-hairy (type1 type2)
  (declare (ignore type1 type2))
  :unknown)

(defmacro def-subtypep-too-hairy (number name)
  `(push (cons ',name (function always-too-hairy))
	 ,(if (= number 1)
	      '*stp-specialist-1-table*
	      '*stp-specialist-2-table*)))

;;; Call-subtypep-specialist returns a form which looks up a specialist
;;;  function and calls it.  number specifies whether to call the type1
;;;  specialist or the type2 specialist.

(defmacro call-subtypep-specialist (number type1 type2)
  (if (= number 1)
      `(let ((fun (cdr (assoc (car ,type1) *stp-specialist-1-table*))))
	 (if fun
	     (funcall fun ,type1 ,type2)
	     :unknown))
      `(let ((fun (cdr (assoc (car ,type2) *stp-specialist-2-table*))))
	 (if fun
	     (funcall fun ,type1 ,type2)
	     :unknown))))

;;;    Functions, Satisfies, Members, Ands, Ors, Nots

(def-subtypep-too-hairy 1 function)
(def-subtypep-too-hairy 2 function)

(def-subtypep-too-hairy 1 satisfies)
(def-subtypep-too-hairy 2 satisfies)

;;; Member

(def-subtypep-specialist 1 member
  (if (every #'(lambda (x) (typep x type2)) (cdr type1))
      :true
      :false))

(def-subtypep-too-hairy 2 member)

;;; (subtypep '(and t1 t2 ...) 't3) <=>
;;; (or (subtypep 't1 't3) (subtypep 't2 't3) ...  (too-hairy))
;;; because '(and t1 t2 ...) denotes the intersection of types t1, t2, ...
;;;
;;; We can't ever return :false because the intersection might
;;; be a subtype even if none of the components are.
;;;

(def-subtypep-specialist 1 and
  (dolist (type1 (cdr type1) :unknown)
    (when (eq (sub-subtypep type1 type2) :true)
      (return :true))))

;;; (subtypep 't1 '(and t2 t3 ...)) <=>
;;; (and (subtypep 't1 't2) (subtypep 't1 't3) ...)
;;; because '(and t2 t3 ...) denotes the intersection of types t2, t3, ...

(def-subtypep-specialist 2 and
  (dolist (type2 (cdr type2) :true)
    (unless (eq (sub-subtypep type1 type2) :true)
      (return :false))))

;;; (subtypep '(or t1 t2 ...) 't3) <=>
;;; (and (subtypep 't1 't3) (subtypep 't2 't3) ...)
;;; because '(or t1 t2 ...) denotes the union of types t1, t2, ...

(def-subtypep-specialist 1 or
  (dolist (type1 (cdr type1) :true)
    (unless (eq (sub-subtypep type1 type2) :true)
      (return :false))))

;;; (subtypep 't1 '(or t2 t3 ...)) <=>
;;; (or (subtypep 't1 't2) (subtypep 't1 't3) ...  (too-hairy))
;;; because '(or t1 t2 ...) denotes the union of types t1, t2, ...
;;;
;;; We can't ever return :false because the t2..tn might
;;; form an exhaustive partition of t1, i.e. 
;;; (subtypep 'float '(or short-float long-float))
;;;

(def-subtypep-specialist 2 or
  (dolist (type2 (cdr type2) :unknown)
    (when (eq (sub-subtypep type1 type2) :true)
      (return :true))))

;;;
;;; NOT really needs to do more checking for
;;;  type disjointness, etc.
;;;

(defun disjointp (t1 t2)
  (and (not (equal t1 t2))
       (memq 'future (list t1 t2))
       (not (or (subtypep t1 t2)
		(subtypep t2 t1)))))

;;; (subtypep '(not t1) t2) <=> (not (subtypep 't1 't2))

(def-subtypep-specialist 1 not
  (cond
   ((and (eq type1 nil) 
	 (eq type2 t))
    :true)
   (t :false)))

;;; (subtypep t1 '(not t2)) <=> (not (subtypep 't1 't2))

(def-subtypep-specialist 2 not
  (if (eq type2 nil)
      :true
      (let ((r (sub-subtypep type1 (cadr type2))))
	(cond
	 ((eq r :unknown) :unknown)
	 ((eq r :true) :false)
	 ((eq r :false)
	  (if (disjointp type1 (cadr type2))
	      :true
	      :unknown))))))


;;; ST-Range>= is used for comparing the lower limits of subranges of numbers.
;;;  St-range>= returns T if n1 >= n2, or if the range whose lower limit is
;;;  n1 is within a range whose lower limit is n2.  N1 and n2 may take on one
;;;  of three distinct types of values:   A number is an inclusive lower bound,
;;;  a list of a number is an exclusive lower bound, and the symbol * 
;;;  represents  minus infinity which is not greater than any other number.
;;;
;;; (st-range>= '(3) 3) => T,    (st-range>= 3 '(3)) => ().

(defun st-range>= (n1 n2)
  (cond
   ((eq n2 '*) T)			;anything is >= -inf.
   ((eq n1 '*) ())			; -inf not >= anything else.
   ((listp n1)
    (if (listp n2)
	(>= (car n1) (car n2))
	(>= (car n1) n2)))
   (T (if (listp n2)
	  (> n1 (car n2))		;this case must be strictly greater than
	  (>= n1 n2)))))

;;; St-range<= is like St-range>= except that it is used to compare upper
;;;  bounds.  It returns true iff n1 is the upper bound of a range which is
;;;  within the range bounded by n2.  Here, * represents + infinity which is
;;;  not less than any other number.

(defun st-range<= (n1 n2)
  (cond
   ((eq n2 '*) T)			;anything is <= +inf
   ((eq n1 '*) ())			; +inf is not <= anything else
   ((listp n1)
    (if (listp n2)
	(<= (car n1) (car n2))
	(<= (car n1) n2)))
   (T (if (listp n2)
	  (< n1 (car n2))		;this case must be strictly less than.
	  (<= n1 n2)))))

;;;    Integers

;;; Inclusivate-Arg  --  Internal
;;;
;;;    Make an integer type arg inclusive.
;;;

(defun inclusivate-arg (type arg)
  (let ((n (type-arg type arg)))
    (cond ((eq n '*) '*)
	  ((consp n)
	   (if (integerp (car n))
	       (1- (car n))
	       (error "Bound is not an integer: ~S" type)))
	  ((not (integerp n))
	   (error "Bound is not an integer: ~S" type))
	  (t n))))

(def-subtypep-specialist 1 integer
  (let ((low1 (inclusivate-arg type1 0))
	(high1 (inclusivate-arg type1 1)))
    (cond
     ((eq (sub-subtypep 'integer type2) :true)
      :true)
     ((eq type2 'fixnum)
      (sub-subtypep type1 `(integer ,most-negative-fixnum ,most-positive-fixnum)))
     ((eq type2 'bignum) :true)
     ((eq type2 'bit) (sub-subtypep type1 '(integer 0 1)))
     ((symbolp type2) :false)

     ;; integer versus integer
     ((eq (car type2) 'integer)
      (let ((low2 (inclusivate-arg type2 0))
	    (high2 (inclusivate-arg type2 1)))
	(if (and (st-range>= low1 low2) ;T if range1 is within
		     (st-range<= high1 high2)) ; range2
	    :true
	    :false)))
	
     ;; integer versus rational
     ((eq (car type2) 'rational)
      (sub-subtypep `(rational ,low1 ,high1) type2))

     ;; Otherwise, maybe the specialist for type2 can help
     (T (call-subtypep-specialist 2 type1 type2))
     )))

;; specialist for the case where type2 is (integer ...)
(def-subtypep-specialist 2 integer
  (cond ((eq type1 'bit)
	 (sub-subtypep '(integer 0 1) type2))
	((symbolp type1)
	 :false)
	(t
	 :unknown)))

;;;    Rationals

(def-subtypep-specialist 1 rational
  (let ((low1 (type-arg type1 0))
	(high1 (type-arg type1 1)))
    (cond
     ((eq (sub-subtypep 'rational type2) :true)
      :true)
     ((symbolp type2) :false)

     ;; rational to rational
     ((eq (car type2) 'rational)
      (let ((low2 (type-arg type2 0))
	    (high2 (type-arg type2 1)))
	(if (and (st-range>= low1 low2)	;T if type1 range is within
	     (st-range<= high1 high2))	; type2 range.
	    :true
	    :false)))

     ;; otherwise maybe the specialist for type2 can help
     (T (call-subtypep-specialist 2 type1 type2)))))

(def-subtypep-specialist 2 rational
  (declare (ignore type2))
  (cond ((symbolp type1) :false)
	(T :unknown)))

;;;    Floats

(def-subtypep-specialist 1 float
  (let ((low1 (type-arg type1 0))
	(high1 (type-arg type1 1)))
    (cond
     ((eq (sub-subtypep 'float type2) :true)
      :true)
     ((eq type2 'short-float)
      (sub-subtypep type1 `(float ,most-negative-short-float
			      ,most-positive-short-float)))
     ((eq type2 'single-float)
      (sub-subtypep type1 `(float ,most-negative-single-float
			      ,most-positive-single-float)))
     ((eq type2 'double-float)
      (sub-subtypep type1 `(float ,most-negative-double-float
			      ,most-positive-double-float)))
     ((eq type2 'long-float)
      (sub-subtypep type1 `(float ,most-negative-long-float
			      ,most-positive-long-float)))
     ((symbolp type2) :false)

     ;; float to float
     ((eq (car type2) 'float)
      (let ((low2 (type-arg type2 0))
	    (high2 (type-arg type2 1)))
	(if (and (st-range>= low1 low2)	;T if type1 range is within
	     (st-range<= high1 high2))	; type2 range.
	    :true
	    :false)))

     ;; otherwise maybe the specialist for type2 can help
     (T (call-subtypep-specialist 2 type1 type2)))))

(def-subtypep-specialist 2 float
  (declare (ignore type2))
  (cond ((symbolp type1) :false)
	(T :unknown)))

;;;    Float types

(def-subtypep-specialist 1 (short-float single-float)
  (cond
   ((eq (sub-subtypep 'short-float type2) :true)
    :true)
   ((member (type-name type2) '(double-float long-float)) 
    :false)
   (T (sub-subtypep `(float . ,(cdr type1)) type2))))

(def-subtypep-specialist 2 (short-float single-float)
  (declare (ignore type2))
  (if (symbolp type1)
      :false
      :unknown))

(def-subtypep-specialist 1 (double-float long-float)
  (cond
   ((eq (sub-subtypep 'long-float type2) :true)
    :true)
   ((member (type-name (type-name type2)) '(short-float single-float)) 
    :false)
   (T (sub-subtypep `(float . ,(cdr type1)) type2))))

(def-subtypep-specialist 2 (double-float long-float)
  (declare (ignore type2))
  (if (symbolp type1)
      :false
      :unknown))

;;;    Complex numbers

(def-subtypep-specialist 1 complex
  (cond

   ;;(complex ...) is a subtype of any type that COMPLEX is a subtype of,
   ((eq (sub-subtypep 'complex type2) :true)
    :true) 
   ;;but not a subtype of any symbol type that COMPLEX is not a subtype of.
   ((symbolp type2) :false)
   ;;Case where Type2 is another complex
   ((eq (car type2) 'complex)
    (sub-subtypep (type-arg type1) (type-arg type2)))
   ;;punt to specialist for type2
   (T (call-subtypep-specialist 2 type1 type2))))

;; specialist for the case where type2 is (complex ...)

(def-subtypep-specialist 2 complex
  (declare (ignore type2))
  (cond ((symbolp type1) :false)
	(T :unknown)))

;;;    Mods, Signed-Bytes, Unsigned-Bytes

;; these forms all turn different flavors of (integer ...) into something
;; that the specialist for integer can understand.

(def-subtypep-specialist 1 mod
  (sub-subtypep `(integer 0 ,(1- (nth 1 type1))) type2))

(def-subtypep-specialist 2 mod
  (sub-subtypep type1 `(integer 0 ,(1- (nth 1 type2)))))

(def-subtypep-specialist 1 signed-byte
  (let ((arg (type-arg type1 0 nil)))
    (if arg
	(let ((highest (ldb (byte (1- arg) 0) -1)))	  ;gets n-1 bits of 1's
	  (sub-subtypep `(integer ,(1- (- highest)) ,highest) type2))
	(sub-subtypep 'integer type2))))

(def-subtypep-specialist 2 signed-byte
  (let ((arg (type-arg type2 0 nil)))
    (if arg
	(let ((highest (ldb (byte (1- arg) 0) -1)))	  ;gets n-1 bits of 1's
	  (sub-subtypep type1 `(integer ,(1- (- highest)) ,highest)))
	(sub-subtypep type1 'integer))))

(def-subtypep-specialist 1 unsigned-byte
  (let ((arg (type-arg type1 0 nil)))
    (if arg
	(let ((highest (ldb (byte arg 0) -1)))	  ;gets n bits of 1's
	  (sub-subtypep `(integer 0 ,highest) type2))
	(sub-subtypep '(integer 0) type2))))

(def-subtypep-specialist 2 unsigned-byte
  (let ((arg (type-arg type2 0 nil)))
    (if arg
	(let ((highest (ldb (byte arg 0) -1)))	  ;gets n bits of 1's
	  (sub-subtypep type1 `(integer 0 ,highest)))
	(sub-subtypep type1 '(integer 0)))))

;;; Array hacking helping functions

;;; St-Array-Dimensions-Encompass returns true iff the first array dimension
;;; specifier is the same as, or more specific than, the second array
;;; dimension specifier.

(defun st-array-dimensions-encompass (first-spec second-spec)
  (cond ((eq second-spec '*)
	 t)
	((integerp second-spec)
	 (cond ((eq first-spec '*)
		:false)
	       ((integerp first-spec)
		(if 
		 (= second-spec first-spec)
		 :true
		 :false))
	       ((listp first-spec)
		(if
		 (= second-spec (length first-spec))
		 :true
		 :false))
	       (t
		:unknown)))
	((listp second-spec)
	 (cond ((eq first-spec '*)
		:false)
	       ((integerp first-spec)
		(do ((second-spec second-spec (cdr second-spec)))
		    ((null second-spec) :true)
		  (if (not (eq (car second-spec) '*))
		      (return :false))))
	       ((listp first-spec)
		(do ((second-spec second-spec (cdr second-spec))
		     (first-spec first-spec (cdr first-spec)))
		    ((or (null second-spec) (null first-spec))
		     (if (and (null second-spec) (null first-spec))
			 :true
			 :false))
		  (if (not (or (eq (car second-spec) '*)
			       (eq (car second-spec) (car first-spec))))
		      (return :false))))
	       (t
		:unknown)))
	(t
	 :unknown)))

'$split-file

;;; St-Array-Element-Type determines the element type of an array specified.

(defun st-array-element-type (spec)
  (if (symbolp spec)
      (case spec
	((array vector simple-array) '*)
	(simple-vector t)
	((bit-vector simple-bit-vector) 'bit)
	((string simple-string) 'string-char)
	(t nil))
      (case (car spec)
	((array vector simple-array)
	 (if (cadr spec)
	     (let ((etype (type-expand (cadr spec))))
	       (cond ((subtypep etype 'bit) 'bit)
		     ((subtypep etype 'string-char) 'string-char)
		     (t etype)))
	     '*))
	(simple-vector t)
	((bit-vector simple-bit-vector) 'bit)
	((string simple-string) 'string-char)
	(t nil))))

;;; Array-Element-Subtypep  --  Internal
;;;
;;;    Returns true if the type components of the array types type1 and type2
;;; do not prohibit Type1 being a subtype of Type2.  This is true when:
;;;  1] The arrays are of the same specialized type: string, bit-vector...
;;;  2] The element type of Type2 is *.
;;;  3] They are not Common Lisp specialized array types, and Type1
;;;     is subtype to Type2.
;;;

(defun array-element-subtypep (type1 type2)
  (let ((et1 (st-array-element-type type1))
	(et2 (st-array-element-type type2)))
    (if (or (eq et2 '*)
	    (eq et1 et2)
	    (and (not (member et1 '(bit string-char t)))
		 (not (member et2 '(bit string-char t)))
		 (eq (sub-subtypep et1 et2) :true)))
	:true
	:false)))

(defun st-array-dimensions (spec)
  (if (symbolp spec)
      (case spec
	((array simple-array) '*)
	((vector simple-vector bit-vector simple-bit-vector
		 string simple-string)
	 '(*))
	(t nil))
      (case (car spec)
	((array simple-array) (or (caddr spec) '*))
	((vector simple-vector) (if (caddr spec) (list (caddr spec)) '(*)))
	((bit-vector simple-bit-vector string simple-string)
	 (if (cadr spec) (list (cadr spec)) '(*)))
	(t nil))))

;;;    Array specialists

;;; For some array type to be a subtype of another, the following
;;; things must be true:
;;; the major type of type2 must have the same "simpleness" as the major
;;;  type of type1,
;;; the dimensions of type2 must be encompassed by the dimensions of
;;;  of type1.
;;; the element type must permit the subtype relation.  See 
;;; Array-Element-Subtypep.

;;; For the case where type1 is (array ...)

(def-subtypep-specialist 1 (array simple-array vector simple-vector
			    bit-vector simple-bit-vector
			    string simple-string)
  (let ((type2-major (or (and (listp type2) (car type2)) type2)))
    (if (and (if (memq type2-major '(simple-array simple-vector
						  simple-bit-vector simple-string))
		 (memq (car type1) '(simple-array simple-vector
						  simple-bit-vector simple-string))
		 t)
	     (eq (array-element-subtypep type1 type2) :true)
	     (eq (st-array-dimensions-encompass (st-array-dimensions type1)
						(st-array-dimensions type2))
		 :true))
	:true
	:false)))
	

;;; For the case where type2 is (array ...)

(def-subtypep-specialist 2 (array simple-array vector simple-vector
			    bit-vector simple-bit-vector
			    string simple-string)
  (let ((type1-major (or (and (listp type1) (car type1)) type1)))
    (if (and (if (memq (car type2) '(simple-array simple-vector
						  simple-bit-vector simple-string))
		 (memq type1-major '(simple-array simple-vector
						  simple-bit-vector simple-string))
		 t)
	     (eq (array-element-subtypep type1 type2) :true))
	:true
	:false)))

;;; *Symbol-subtype-table*

;;; The symbol-subtypep-table is a list containing one entry per known symbol
;;;  type.  Each entry is a list of symbols which are all subtypes of the car 
;;;  of the list.  To test whether b is a subtype of a, find the list 
;;;  beginning with a, and then see whether b is in it.

(defvar *symbol-subtype-table*
  '((* array atom bignum bit bit-vector character common compiled-function
       complex cons double-float fixnum float function hash-table integer
       keyword list long-float nil null number package pathname
       random-state ratio rational readtable sequence short-float
       simple-array simple-bit-vector simple-string simple-vector
       single-float standard-char stream string string-char symbol t
       vector future)
    (t array atom bignum bit bit-vector character common compiled-function
       complex cons double-float fixnum float function hash-table integer
       keyword list long-float nil null number package pathname
       random-state ratio rational readtable sequence short-float
       simple-array simple-bit-vector simple-string simple-vector
       single-float standard-char stream string string-char symbol t
       vector future)
    (array bit-vector simple-array simple-bit-vector simple-string
	   simple-vector string vector)
    (atom array bignum bit bit-vector character common compiled-function
	  complex double-float fixnum float function hash-table integer
	  keyword long-float nil null number package pathname
	  random-state ratio rational readtable sequence short-float
	  simple-array simple-bit-vector simple-string simple-vector
	  single-float standard-char stream string string-char symbol
	  vector)
    (bignum)
    (bit)
    (bit-vector simple-bit-vector)
    (character standard-char string-char)
    (common array atom bignum bit bit-vector character common compiled-function
	    complex cons double-float fixnum float function hash-table integer
	    keyword list long-float nil null number package pathname
	    random-state ratio rational readtable sequence short-float
	    simple-array simple-bit-vector simple-string simple-vector
	    single-float standard-char stream string string-char symbol
	    vector)
    (compiled-function)
    (complex)
    (cons)
    (double-float)
    (fixnum bit)
    (float double-float long-float short-float single-float)
    (function compiled-function symbol)
    (future)
    (hash-table)
    (integer bignum fixnum bit)
    (keyword)
    (list cons null)
    (long-float)
    (nil)
    (null)
    (number bignum bit complex double-float fixnum float integer long-float
	    ratio rational short-float single-float)
    (package)
    (pathname)
    (random-state)
    (ratio)
    (rational bignum bit fixnum integer ratio)
    (readtable)
    (sequence array bit-vector list simple-array simple-bit-vector
	      simple-string simple-vector string vector)
    (short-float)
    (simple-array simple-bit-vector simple-string simple-vector)
    (simple-bit-vector)
    (simple-string)
    (simple-vector)
    (single-float)
    (standard-char)
    (stream)
    (string simple-string)
    (string-char standard-char)
    (symbol keyword null)
    (vector bit-vector simple-bit-vector simple-string simple-vector
	    string)))

;;; Sub-Subtypep

;;; Sub-Subtypep returns T if TYPE1 is a subtype of TYPE2, () if it is not.
;;;  Some cases can not be decided.  If this occurs, :unknown is returned.
;;;
;;; If type1 is a list, then call a specialized function which handles that
;;;  particular list type when it appears as type1.  Otherwise, if type2 is a
;;;  list, then call the specialist which handles that particular list type
;;;  when it appears as type2.
;;;
;;; If both types are symbols, then lookup the subtype relation in the
;;;  *symbol-subtype-table*. 
;;;
;;; Specialist functions are associated with the name of the car of the list
;;;  types.  The macro get-subtype-specialist is used look up the function.
;;;  The numeric arg specifies whether to get the specialist for the type1
;;;  case or the type2 case.

(defconstant *subtype-cache-size* 1024)

(defvar *subtype-cache*)
(setq *subtype-cache* (make-vector *subtype-cache-size* '()))

(defvar *use-subtype-cache*)
(setq *use-subtype-cache* nil)

(defun clear-subtype-cache ()
  (setq *subtype-cache* (make-vector *subtype-cache-size* '()))
  (clear-subtract-futures-cache))

(defun sub-subtypep (type1 type2)
  (if *use-subtype-cache*
      (let ((key (cons type1 type2)))
	(let ((index (mod (sxhash key) *subtype-cache-size*)))
	  (let ((bucket (system-vector-ref *subtype-cache* index)))
	    (let ((cached-entry
		   (if bucket
		       (scheme-assoc key bucket)
		       bucket)))
	      (if cached-entry
		  (cdr cached-entry)
		  (let ((result (sub-subtypep1 type1 type2)))
		    (system-vector-set! *subtype-cache* index
					(cons (cons key result) bucket))
		    result))))))
      (sub-subtypep1 type1 type2)))

(defun sub-subtypep1 (type1 type2)
  (let ((type1 (type-expand type1))
	(type2 (type-expand type2)))
    (cond
     ((equal type1 type2) :true)
     ((eq type2 '*) :true)
     ((and (eq type2 t) (not (eq type1 '*))) :true)
     ((eq type1 nil) :true)
     ((listp type1)
      (call-subtypep-specialist 1 type1 type2))
     ((listp type2)
      (call-subtypep-specialist 2 type1 type2))
     ((not (assq type1 *symbol-subtype-table*))
      :unknown)
     (T
      (let ((subtypes-of-2  (assq type2 *symbol-subtype-table*)))
	(if (null subtypes-of-2)
	    :unknown)
	(if (member type1 subtypes-of-2)
	    :true
	    :false)
	))
     )))

;;; Subtypep

;;; Subtypep returns two values which may be any of the three following pairs.
;;;
;;;   T T    -- TYPE1 is a subtype of TYPE2.
;;;   () T   -- TYPE1 is not a subtype of TYPE2.
;;;   () ()  -- Couldn't tell.
;;;
;;; Sub-subtypep returns either :true, :false, or :unknown;
;;;  translated appropriately to values by subtypep.

(defun subtypep (type1 type2)
  "Returns T if type1 is a subtype of type2.  If second value is (), couldn't
  decide."
  (subtype-values (sub-subtypep (touch type1) (touch type2))))

(defun subtype-values (x)
  (case x
    (:true (values t t))
    (:false (values nil t))
    (:unknown (values nil nil))
    (t (error "Internal error: bad value ~a to subtype-values" x))))

;;; For use by the syntaxer

(set! subtypep (symbol-function 'subtypep))
