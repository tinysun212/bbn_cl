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
(eval-when (compile)
	   (load "mini-defstruct.bin" ()))

;;; It is assumed that defstruct name and options and
;;; slot descriptions contain no futures, except perhaps
;;; at the top level. 

(proclaim '(insert-touches nil))

(export '(defstruct))

;; A defstruct is implemented with two defstructs, one
;; which defines the defstruct and the other which defines
;; each of the defstruct slots.  For bootstrapping purposes,
;; we use mini-defstruct for these structures.

;;; The description of a defstruct 

(mini-defstruct
  (defstruct-description
    name			; name of the structure
    doc				; documentation on the structure
    slots			; list of slots
    conc-name			; prefix for slot names
    constructor			; name of standard constructor function
    boa-constructors		; name of by-position constructors
    copier			; name of copying function
    predicate			; name of type predictate
    include			; name of included structure
    includes			; names of all structures included by this one
    print-function		; function used to print it
    type			; type specified, Structure if no type specified.
    lisp-type			; actual type used for implementation.
    named			; T if named, Nil otherwise
    offset			; first slot's offset into implementation sequence
    length)			; total length of the thing
  dd)                           ; prefix for defstruct-description fields

;;; The description of a defstruct slot

(mini-defstruct
 (defstruct-slot-description
   %name			; string name of slot
   index			; its position in the implementation sequence
   accessor			; name of it accessor function
   default			; default value
   type				; declared type
   read-only)			; T if there's to be no setter for it
 dsd)                           ; prefix for defstruct-slot-description fields

;;; DSD-Name  --  Internal
;;;
;;;    Return the the name of a defstruct slot as a symbol.  We store it
;;; as a string to avoid creating lots of worthless symbols at load time.
;;;

(defun dsd-name (dsd)
  (intern (string (dsd-%name dsd)) (symbol-package (dsd-accessor dsd))))

'$split-file
;;; Default structures will be created out of g-vectors.

(cl-define make-structure-g-vector (make-primitive-procedure 'make-structure-g-vector))

(defun parse-name-and-options (name-and-options)
  (if (atom name-and-options)
      (setq name-and-options (list name-and-options)))
  (do* ((options (cdr name-and-options) (cdr options))
	(name (car name-and-options))
	(print-function 'default-structure-print)
	(conc-name (concat-pnames name '-))
	(constructor (concat-pnames 'make- name))
	(saw-constructor)
	(boa-constructors '())
	(copier (concat-pnames 'copy- name))
	(predicate (concat-pnames name '-p))
	(include)
	(saw-type)
	(type 'structure)
	(saw-named)
	(offset 0))
       ((null options)
	 
	(make-defstruct-description
	 name
	 '()           ; no doc yet
	 '()           ; no slots yet
	 conc-name
	 constructor
	 boa-constructors
	 copier
	 predicate
	 include
	 '()           ; no includes yet
	 print-function
	 type
	 (cond ((eq type 'structure) 'simple-vector)
	       ((eq type 'vector) 'simple-vector)
	       ((eq type 'list) 'list)
	       ((and (listp type) (eq (car type) 'vector))
		(cons 'simple-array (cdr type)))
	       (t (error "~S is a bad :TYPE for Defstruct." type)))
	 (if saw-type saw-named t)
	 offset
	 '()           ; no length
	 ))
    (if (atom (car options))
	(case (car options)
	  (:constructor (setq saw-constructor t
			      constructor (concat-pnames 'make- name)))
	  (:copier)
	  (:predicate)
	  (:named (setq saw-named t))
	  (t (error "The Defstruct option ~S cannot be used with 0 arguments."
		    (car options))))
	(let ((option (caar options))
	      (args (cdar options)))
	  (case option
	    (:conc-name (setq conc-name (car args)))
	    (:constructor (cond ((cdr args)
				 (unless saw-constructor
				   (setq constructor nil))
				 (push args boa-constructors))
				(t
				 (setq constructor (car args)))))
	    (:copier (setq copier (car args)))
	    (:predicate (setq predicate (car args)))
	    (:include (setq include args))
	    (:print-function (setq print-function (car args)))
	    (:type (setq saw-type t type (car args)))
	    (:named (error "The Defstruct option :NAMED takes no arguments."))
	    (:initial-offset (setq offset (car args)))
	    (t (error "~S is an unknown Defstruct option." option)))))))

'$split-file
;;; Parse-Slot-Descriptions parses the slot descriptions (surprise) and does
;;; any structure inclusion that needs to be done.

(defun parse-slot-descriptions (structure slots)
  ;; First strip off any doc string and stash it in the structure.
  (when (stringp (car slots))
    (setf (dd-doc structure) (car slots))
    (setq slots (cdr slots)))
  ;; Then include stuff.  We add unparsed items to the start of the Slots.
  (when (dd-include structure)
    (let* ((included-name (car (dd-include structure)))
	   (included-thing (system-get included-name '%structure-definition))
	   (modified-slots (cdr (dd-include structure))))
      (unless included-thing
	(error "Cannot find description of structure ~S to use for inclusion."
	       included-name))
      (setf (dd-includes structure)
	    (cons (dd-name included-thing) (dd-includes included-thing)))
      (setf (dd-offset structure) (dd-offset included-thing))
      (do* ((islots (mapcar (function (lambda (slot)
				`(,(dsd-name slot) ,(dsd-default slot)
				  :type ,(dsd-type slot)
				  :read-only ,(dsd-read-only slot))))
			    (dd-slots included-thing)))
	    (islots* islots (cdr islots*)))
	   ((null islots*)
	    (setq slots (nconc islots slots)))
	(let* ((islot (car islots*))
	       (modifiee (find (car islot) modified-slots
			       :key (function (lambda (x) (if (atom x) x (car x))))
			       :test (function string=))))
	  (when modifiee
	    (cond ((symbolp modifiee)
		   ;; If it's just a symbol, nilify the default.
		   (setf (cadr islot) nil))
		  ((listp modifiee)
		   ;; If it's a list, parse new defaults and options.
		   (setf (cadr islot) (cadr modifiee))
		   (when (cddr modifiee)
		     (do ((options (cddr modifiee) (cddr options)))
			 ((null options))
		       (case (car options)
			 (:type
			  (setf (cadddr islot) (cadr options)))
			 (:read-only
			  (setf (cadr (cddddr islot)) (cadr options)))
			 (t
			  (error "Bad option in included slot spec: ~S."
				 (car options)))))))))))))
  ;; Finally parse the slots into Slot-Description objects.
  (do ((slots slots (cdr slots))
       (index (the fixnum (+ (the fixnum (dd-offset structure))
			     (the fixnum (if (dd-named structure) 1 0))))
	      (1+ index))
       (descriptions '()))
      ((null slots)
       (setf (dd-length structure) index)
       (setf (dd-slots structure) (nreverse descriptions)))
    (declare (fixnum index))
    (let ((slot (car slots)))
      (push
       (if (atom slot)
	   (let ((name slot))
	     (make-defstruct-slot-description
	      (string name)
	      index
	      (concat-pnames (dd-conc-name structure) name)
	      '()          ; No default
	       t           ; type is t
	       '()         ; not read only
	       ))
	   (do ((options (cddr slot) (cddr options))
		(name (car slot))
		(default (cadr slot))
		(type t)
		(read-only nil))
	       ((null options)
		(make-defstruct-slot-description
		 (string name)
		 index
		 (concat-pnames (dd-conc-name structure) name)
		 default
		 type
		 read-only))
	     (case (car options)
	       (:type (setq type (cadr options)))
	       (:read-only (setq read-only (cadr options))))))
       descriptions))))

(defun concat-pnames (name1 name2)
  (if name1
      (intern (concatenate 'simple-string (symbol-name name1) (symbol-name name2)))
      name2))

'$split-file

;;; The built in copier for the default kind of structure:

(defun built-in-copier (old)
  (do* ((index 0 (1+ index))
	(length (g-vector-length old))
	(new (make-structure-g-vector length nil)))
       ((= index length) new)
    (declare (fixnum index length))
    (g-vector-set! new index (g-vector-ref old index))))


;;; Accessors and Setters are emitted as calls to the generic
;;;  functions, structure-ref and structure-set!, resepctively,
;;;  possibly prefixed by "notinline".
;;; In addition, they are proclaimed "structure-operation",
;;;  with the appropriate parameters.
;;; The decl processor in the syntaxer senses the proclaimed names and,
;;;  if not declared notinline, replaces them with structure[ref/set!] calls.
;;;  The compiler knows how to turn these calls into inline code.
;;; To avoid inlining of the calls to the structure[ref/set!] from the
;;;  emitted functions themselves, these are renamed with the notinline prefix.

;;; Structure-operation declaration format:
;;;
;;;  (structure-operation <op-name> <op-type> <defstruct-descriptor> <defstruct-slot-descriptor>)
;;;
;;;    <op-type> -> accessor | setter
;;;
;;; Note that the structure and sot descriptors are constant, shared vectors,
;;;  and so should not cause a space problem.

(defun define-accessors (dd)
  (let ((stuff '()))
    (do ((slots (dd-slots dd) (cdr slots)))
	((null slots) stuff)
      (let* ((dsd (car slots))
	     (name (dsd-accessor dsd)))
	(push
	 `(progn
	    (proclaim
	     '(structure-operation ,name accessor ,dd ,dsd))
	    (defun ,name (structure)
	      (notinline-structure-ref structure ',dd ',dsd)))
	 stuff)))))

;;; We add the "notinline" prefix to the standard name so that it doesn't
;;; get picked up and inlined by the compiler.

(defun notinline-structure-ref (structure dd dsd)
  (let* ((named (dd-named dd))
	 (struct-name (and named (dd-name dd)))
	 (kind (dd-type dd))
	 (type (if (eq kind 'structure) struct-name kind))
	 (index (dsd-index dsd))
	 (offset (dd-offset dd))
	 (name (dsd-accessor dsd)))
    (declare (fixnum index offset))
    (labels ((error ()
		    (structure-type-error name "accessor" structure struct-name type kind)))
      (cond
       ((eq kind 'structure)
	(if (typep structure type)
	    (system-vector-ref structure (the fixnum (1+ index)))
	    (error)))
       ((or (eq kind 'vector)		; simple-vector or subtype implemented as such
	    (subtypep '(vector symbol) kind))
	(if (and (eq (type-of structure) 'vector)
		 (or (not struct-name)
		     (and (> (length structure) (the fixnum (1+ offset)))
			  (eq (svref structure offset) name))))
	    (svref structure index)
	    (error)))
       ((eq kind 'list)
	(if (and (memq (type-of structure) '(cons null)) ; typep avoided for efficiency
		 (or (not struct-name)
		     (eq (nth offset structure) struct-name)))
	    (list-ref structure index)
	    (error)))
       (t				; it's a sequence that cannot be named
	(if (typep structure type)
	    (elt structure index)
	    (error)))))))

;;; The interpreted behavior is the same as for the inline case.
;;; Calls to this are emitted by the syntaxer when an accessor
;;; is not notinlined. The compiler then open codes it.

(setq structure-ref (symbol-function 'notinline-structure-ref))



'$split-file

(defun define-setters (dd)
  (let ((stuff '()))
    (do ((slots (dd-slots dd) (cdr slots)))
	((null slots) stuff)
      (let* ((dsd (car slots))
	     (name (intern (string-append "%SET-" (symbol-name (dsd-accessor dsd))))))
	(unless (dsd-read-only dsd)
	  (push
	   `(progn
	      (proclaim
	       '(structure-operation ,name setter ,dd ,dsd))
	      (defun ,name (structure value)
		(notinline-structure-set! structure value ',dd ',dsd))
	      (defsetf ,(dsd-accessor dsd) ,name))
	   stuff))))))

;;; We add the "notinline" prefix to the standard name so that it doesn't
;;; get picked up and inlined by the compiler.

(defun notinline-structure-set! (structure value dd dsd)
  (let* ((named (dd-named dd))
	 (struct-name (and named (dd-name dd)))
	 (kind (dd-type dd))
	 (type (if (eq kind 'structure) struct-name kind))
	 (index (dsd-index dsd))
	 (offset (dd-offset dd))
	 (slot-type (dsd-type dsd))
	 (slot-name (dsd-%name dsd))
	 (name (dsd-accessor dsd)))
    (declare (fixnum index offset))
    (if (not (typep value slot-type))
	(structure-slot-type-error structure name struct-name type kind slot-type slot-name value)
	(labels ((error ()
			(structure-type-error name "setter of" structure struct-name type kind)))
	  (cond
	   ((eq kind 'structure)
	    (if (typep structure type)
		(cl-system-vector-set! structure (the fixnum (1+ index)) value)
		(error)))
	   ((or (eq kind 'vector)	; simple-vector or subtype implemented as such
		(subtypep '(vector symbol) kind))
	    (if (and (eq (type-of structure) 'vector)
		     (or (not struct-name)
			 (and (> (length structure) (the fixnum (1+ offset)))
			      (eq (svref structure offset) name))))
		(setf (svref structure index) value)
		(error)))
	   ((eq kind 'list)
	    (if (and (memq (type-of structure) '(cons null)) ; typep avoided for efficiency
		     (or (not struct-name)
			 (eq (nth offset structure) struct-name)))
		(progn
		  (set-car! (list-tail structure index) value)
		  value)
		(error)))
	   (t				; it's a sequence that cannot be named
	    (if (typep structure type)
		(setf (elt structure index) value)
		(error))))))))

(setq structure-set! (symbol-function 'notinline-structure-set!))

(defun structure-type-error (name operation-type structure struct-name type kind)
  (cond
   ((eq kind 'structure)
    (error "Structure ~a ~s was passed ~s, which is ~
           not of structure type ~s"
	   operation-type name structure struct-name))
   (struct-name
    (error "Structure ~a ~s for structure named ~s was passed ~s, ~
           which is not of type ~s"
	   operation-type name struct-name structure type))
   (t 
    (error "Structure ~a ~s was passed ~s, which is ~
           not of type ~s"
	   operation-type name structure type))))

(defun structure-slot-type-error (structure name struct-name type kind slot-type slot-name value)
  (error "Attempt to set slot ~a of structure ~s to ~s; should be of type ~s"
	 slot-name structure value slot-type))

;;; This is called from the syntaxer and compiler.
;;; Cont is called with null arguments if inline code
;;; cannot be generated.

(set! structure-operation-info
      (lambda (attribute-list cont)
	(let* ((op-name (nth 0 attribute-list))
	       (op-type (nth 1 attribute-list))
	       (dd      (nth 2 attribute-list))
	       (dsd     (nth 3 attribute-list))
	       (kind (dd-type dd))
	       (index (if (eq kind 'structure) 
			  (1+ (dsd-index dsd))
			  (dsd-index dsd)))
	       (slot-type (dsd-type dsd)))
	  (if (not (or (eq kind 'structure)
		       (eq kind 'vector)
		       (subtypep '(vector symbol) kind)))
	      (funcall cont #f #f #f #f #f)
	      (funcall cont op-type slot-type index dd dsd)))))

;;; Define-Constructor returns a definition for the constructor function of the
;;; given structure.  If the structure is implemented as a vector and is named,
;;; we structurify it.  If the structure is a vector of some specialized type,
;;; we can't use the Vector function.
;;;
;;; If we are defining safe accessors, we also check the types of the values to
;;; make sure that they are legal.
;;;
(defun define-constructor (structure)
  (let ((name (dd-constructor structure)))
    (when name
      (let* ((initial-cruft
	      (if (dd-named structure)
		  (make-list (1+ (dd-offset structure))
			     :initial-element `',(dd-name structure))
		  (make-list (dd-offset structure))))
	     (slots (dd-slots structure))
	     (arg-names (mapcar (function dsd-name) slots))
	     (args (mapcar (function (lambda (slot)
			       `(,(dsd-name slot) ,(dsd-default slot))))
			   slots)))
	`((defun ,name (&key ,@args)
	    ,(case (dd-type structure)
	       (list
		`(list ,@initial-cruft ,@arg-names))
	       (structure
		(let* ((len (+ (length initial-cruft) (length arg-names))))
		  `(make-structure-g-vector ,len (list ,@initial-cruft ,@arg-names))))
	       (vector
		`(vector ,@initial-cruft ,@arg-names))
	       (t
		(do ((sluts slots (cdr sluts))
		     (sets '())
		     (temp (gensym)))
		    ((null sluts)
		     `(let ((,temp (make-array ,(dd-length structure)
					       :element-type
					       ',(cadr (dd-lisp-type structure)))))
			,@(when (dd-named structure)
			    `(setf (aref ,temp ,(dd-offset structure))
				   ',(dd-name structure)))
			,@sets
			,temp))
		  (let ((slot (car sluts)))
		    (push `(setf (aref ,temp ,(dsd-index slot))
				 ,(dsd-name slot))
			  sets)))))))))))

;;; Find-Legal-Slot   --  Internal
;;;
;;;    Given a defstruct description and a slot name, return the corresponding
;;; slot if it exists, or signal an error if not.
;;;
(defun find-legal-slot (structure name)
  (or (find name (dd-slots structure) :key (function dsd-name))
      (error "~S is not a defined slot name in the ~S structure."
	     name (dd-name structure))))

;;; Define-Boa-Constructors defines positional constructor functions.  We generate
;;; code to set each variable not specified in the arglist to the default given
;;; in the structure.  We just slap required args in, as with rest args and aux
;;; args.  Optionals are treated a little differently.  Those that aren't
;;; supplied with a default in the arg list are mashed so that their default in
;;; the arglist is the corresponding default from the structure.
;;;
;;; If we are defining safe accessors, we check the types of the arguments
;;; supplied.  We don't bother checking the defaulted arguments since we would
;;; have to figure out how to eval the defaults only once, and it probably
;;; isn't worth the effort anyway.

(defun define-boa-constructors (structure)
  (do* ((boas (dd-boa-constructors structure) (cdr boas))
	(name (car (car boas)) (car (car boas)))
	(args (copy-list (cadr (car boas))) (copy-list (cadr (car boas))))
	(slots (dd-slots structure) (dd-slots structure))
	(slots-in-arglist '() '())
	(defuns '()))
       ((null boas) defuns)
    ;; Find the slots in the arglist and hack the defaultless optionals.
    (do ((args args (cdr args))
	 (arg-kind 'required))
	((null args))
      (let ((arg (car args)))
	(if (atom arg)
	    (if (memq arg '(&optional &rest &aux))
		(setq arg-kind arg)
		(case arg-kind
		  ((required &rest &aux)
		   (push (find-legal-slot structure arg) slots-in-arglist))
		  (&optional
		   (let ((dsd (find-legal-slot structure arg)))
		     (push dsd slots-in-arglist)
		     (rplaca args (list arg (dsd-default dsd)))))))
	    (push (find-legal-slot structure (car arg)) slots-in-arglist))))

    ;; Then make a list that can be used with a (list ...) or (vector ...).
    (let ((initial-cruft
	   (if (dd-named structure)
	       (make-list (1+ (dd-offset structure))
			  :initial-element `',(dd-name structure))
	       (make-list (dd-offset structure))))
	  (thing (mapcar (function (lambda (slot)
			     (if (memq slot slots-in-arglist)
				 (dsd-name slot)
				 (dsd-default slot))))
			 slots)))
      (push
       `(defun ,name ,args
	  ,(case (dd-type structure)
	     (list
	      `(list ,@initial-cruft ,@thing))
             (structure
	      (let* ((len (+ (length initial-cruft) (length thing))))
		`(make-structure-g-vector ,len (list ,@initial-cruft ,@thing))))
	     (vector
	      `(vector ,@initial-cruft ,@thing))
	     (t
	      (do ((things thing (cdr things))
		   (index 0 (1+ index))
		   (sets '())
		   (temp (gensym)))
		  ((null things)
		   `(let ((,temp (make-array ,(dd-length structure)
					     :element-type
					     ',(cadr (dd-lisp-type structure)))))
		      ,@(when (dd-named structure)
			  `(setf (aref ,temp ,(dd-offset structure))
				 ',(dd-name structure)))
		      ,@sets
		      ,temp))
		(push `(setf (aref ,temp index) ,(car things))
		      sets)))))
       defuns))))

'$split-file


;;; Define-Copier returns the definition for a copier function for the given
;;; structure if one is desired.  If the structure is implemented as a vector
;;; and is named, we use our Built-In-Copier.

(defun define-copier (structure)
  (if (dd-copier structure)
      (cond ((eq (dd-type structure) 'structure)
	     `((setf (symbol-function ',(dd-copier structure))
		     (symbol-function 'built-in-copier))))
	    ((eq (dd-type structure) 'list)
	     `((setf (symbol-function ',(dd-copier structure))
		     (symbol-function 'copy-list))))
	    (t
	     `((defun ,(dd-copier structure) (structure)
		 (copy-seq (the ,(dd-lisp-type structure) structure))))))))

;;; Define-Predicate returns a definition for a predicate function if one is
;;; desired.  For normal structures, we just use Typep and let the Typep
;;; transform to clever things.  For other named structures, we have to emit
;;; special-case code.

(defun define-predicate (structure)
  (let ((pred (dd-predicate structure)))
    (when (and pred (dd-named structure))
      (let ((type (dd-type structure))
	    (offset (dd-offset structure))
	    (name (dd-name structure)))
	(cond
	 ((eq type 'structure)
	  `((defun ,pred (object)
	      (typep object ',name))))
	 ((subtypep type 'vector)
	  `((defun ,pred (object)
	      (and (subtypep '(vector symbol) (type-of object))
		   (> (length object) ,(1+ offset))
		   (eq (aref object ,offset) ',name)))))
	 ((eq type 'list)
	  `((defun ,pred (object)
	      (and (listp object)
		   (eq (nth ,offset object) ',name)))))
	 (t nil))))))

;;; Vector-Sub-Predicate  --  Internal
;;;
;;;    Do the hairy defstruct type check, taking the inheritance into 
;;; consideration.
;;;
(defun vector-sub-predicate (object type)
  (not (null (memq type (dd-includes (system-get (svref object 0)
						 '%structure-definition))))))

;;; Random sorts of stuff.

(defun default-structure-print (structure stream depth)
  (declare (ignore depth))
  (write-string "#S(" stream)
  (prin1 (g-vector-ref structure 0) stream)
  (do ((index 1 (1+ index))
       (length (g-vector-length structure))
       (slots (dd-slots 
	       (system-get (g-vector-ref structure 0) '%structure-definition))
	      (cdr slots)))
      ((or (= index length)
	   (and *print-length*
		(= index *print-length*)))
       (if (= index length)
	   (write-string ")" stream)
	   (write-string "...)" stream)))
    (declare (fixnum index length))
    (write-char #\sp stream)
    (prin1 (dsd-name (car slots)) stream)
    (write-char #\sp stream)
    (prin1 (g-vector-ref structure index) stream)))

(defun defstruct-internal (thunk)
  (funcall thunk))

(defmacro defstruct (name-and-options &rest slot-descriptions)
  (let* ((structure (parse-name-and-options (touch name-and-options)))
	 (name (dd-name structure))
	 (print-function (dd-print-function structure)))
    (if (not (symbolp print-function))
	(%set-dd-print-function structure nil))
    (parse-slot-descriptions structure (touch slot-descriptions))
    `(defstruct-internal
       (lambda ()
	 ,@(define-accessors structure)
	 ,@(define-setters structure)
	 ,@(define-constructor structure)
	 ,@(define-boa-constructors structure)
	 ,@(define-copier structure)
	 ,@(define-predicate structure)
	 ,@(if (dd-doc structure)
	       `((setf (system-get ',name '%struct-documentation) ',(dd-doc structure))))
	 ,@(if (not (symbolp print-function))
	       `((%set-dd-print-function ',structure ,print-function)))
	 (clear-subtype-cache)
	 (let ((new (copy-defstruct-description ',structure)))
	   (setf (system-get ',name '%structure-definition) new))
	 ',name))))






