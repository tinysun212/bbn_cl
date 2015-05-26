#| -*-Scheme-*-

$Header: dassm1.scm,v 1.3 88/08/31 10:45:04 jinx Exp $
$MIT-Header: dassm1.scm,v 4.3 88/03/14 19:15:45 GMT jinx Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Disassembler: User Level

(declare (usual-integrations))

;;; Flags that control disassembler behavior

(define disassembler/symbolize-output? true)
(define disassembler/compiled-code-heuristics? true)
(define disassembler/write-offsets? true)
(define disassembler/write-addresses? false)

;;;; Top level entries

(define (compiler:write-lap-file filename #!optional symbol-table?)
  (let ((pathname (->pathname filename)))
    (with-output-to-file (pathname-new-type pathname "lap")
      (lambda ()
	(let ((com-objects
	       (fasload-multiple
		(pathname-new-type pathname "com")))
	      (binf-objects
	       (or (let ((pathname (pathname-new-type pathname "binf")))
		     (and (if (unassigned? symbol-table?)
			      (file-exists? pathname)
			      symbol-table?)
			  (map 
			   (lambda (x)
			     (if (and (pair? x)
				      (eq? (car x) 'binf-list))
				 (map compiler-info/symbol-table
				      (cdr x))
				 (list (compiler-info/symbol-table x))))
			   (compiler-info/read-file pathname))))
		   (circular-list '()))))
	  (for-each 
	   (lambda (com-object binf-object)
	     (if (sequence? com-object)
		 (sequence-components com-object
		   (lambda (com-object-list)
		     (for-each 
		      (lambda (x y)
			(disassembler/write-compiled-code-block (compiled-code-address->block x) y))
		      com-object-list
		      binf-object)))
		 (disassembler/write-compiled-code-block 
		  (compiled-code-address->block com-object)
		  (car binf-object))))
	   com-objects
	   binf-objects))))))

(define disassembler/base-address)

;;; This should be fixed.  It only works if the block is the first block
;;; in the file of a multi block fasl file, and if the entry point is an
;;; entry point into the block, rather than (for example) a non-trivial
;;; compiled closure, or a fake uuo-link.

(define (disassembler/write-compiled-entry entry)
  (let ((the-block (compiled-code-address->block entry)))
    (fluid-let ((disassembler/write-offsets? true)
		(disassembler/write-addresses? true)
		(disassembler/base-address (primitive-datum the-block)))
      (let ((info
	     (compiler-info/read-file
	      (system-vector-ref the-block
				 (-  (system-vector-size the-block) 2)))))
	(newline)
	(newline)
	(disassembler/write-compiled-code-block
	 the-block
	 (compiler-info/symbol-table info))))))

;;; Operations exported from the disassembler package

(define disassembler/instructions)
(define disassembler/instructions/null?)
(define disassembler/instructions/read)
(define disassembler/lookup-symbol)

(define (disassembler/write-compiled-code-block block symbol-table)
  (write-string "Code:\n\n")
  (disassembler/write-instruction-stream
   symbol-table
   (disassembler/instructions/compiled-code-block block symbol-table))
  (write-string "\nConstants:\n\n")
  (disassembler/write-constants-block block symbol-table))

(define (disassembler/instructions/compiled-code-block block symbol-table)
  (disassembler/instructions block
			     (compiled-code-block/code-start block)
			     (compiled-code-block/code-end block)
			     symbol-table))

(define (disassembler/instructions/address start-address end-address)
  (disassembler/instructions false start-address end-address false))

(define (disassembler/write-instruction-stream symbol-table instruction-stream)
  (fluid-let ((*unparser-radix* 16))
    (disassembler/for-each-instruction instruction-stream
      (lambda (offset instruction)
	(disassembler/write-instruction
	 symbol-table
	 offset
	 (lambda ()
	   (let ((string
		  (with-output-to-string
		    (lambda ()
		      (display instruction)))))
	     (string-downcase! string)
	     (write-string string))))))))

(define (disassembler/for-each-instruction instruction-stream procedure)
  (let loop ((instruction-stream instruction-stream))
    (if (not (disassembler/instructions/null? instruction-stream))
	(disassembler/instructions/read instruction-stream
	  (lambda (offset instruction instruction-stream)
	    (procedure offset instruction)
	    (loop (instruction-stream)))))))

(define disassembler/write-constants-block)
(let ()

(set! disassembler/write-constants-block
  (named-lambda (disassembler/write-constants-block block symbol-table)
    (fluid-let ((*unparser-radix* 16))
      (let ((end (system-vector-size block)))
	(let loop ((index (compiled-code-block/constants-start block)))
	  (if (< index end)
	      (begin
		(disassembler/write-instruction
		 symbol-table
		 (compiled-code-block/index->offset index)
		 (lambda ()
		   (write-constant block
				   symbol-table
				   (system-vector-ref block index))))
		(loop (1+ index)))))))))

(define (write-constant block symbol-table constant)
  (write-string (cdr (write-to-string constant 60)))
  (if (lambda? constant)
      (let ((expression (lambda-body constant)))
	(if (and (compiled-code-address? expression)
		 (eq? (compiled-code-address->block expression) block))
	    (begin
	      (write-string "  (")
	      (let ((offset (compiled-code-address->offset expression)))
		(let ((label (disassembler/lookup-symbol symbol-table offset)))
		  (if label
		      (write-string (string-downcase label))
		      (write offset))))
	      (write-string ")"))))))

)

(define (disassembler/write-instruction symbol-table offset write-instruction)
  (if symbol-table
      (sorted-vector/for-each symbol-table offset
	(lambda (label)
	  (write-char #\Tab)
	  (write-string (string-downcase (label-info-name label)))
	  (write-char #\:)
	  (newline))))

  (if disassembler/write-addresses?
      (begin
	(write-string
	 ((access unparse-number-heuristically number-unparser-package)
	  (+ offset disassembler/base-address) 16 false false))
	(write-char #\Tab)))
  
  (if disassembler/write-offsets?
      (begin
	(write-string
	 ((access unparse-number-heuristically number-unparser-package)
	  offset 16 false false))
	(write-char #\Tab)))

  (if symbol-table
      (write-string "    "))
  (write-instruction)
  (newline))
