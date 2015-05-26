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

(declare (usual-integrations))

;;;; Imperative style (BLOCK and TAGBODY) Scode optimizer.
;;;
;;; Scode to Scode transformation which transforms RETURN-FROM and GO into
;;; tail recursions whenever possible.
;;;
;;; Currently it depends on recognizing the expansion of BLOCK and TAGBODY,
;;; but it could be generalized to other uses of CWCC and friends.
;;;
;;;; Top level

(set! optimize-imperative-constructs

(named-lambda (optimize-imperative-constructs expr)
  (with-top-level-continuation
   (lambda (cont)
     (optimize/expression expr cont
			  (lambda (fexp iexp subpr reduc)
			    (if subpr
				(error "optimize: subpr" subpr expr))
			    (if (not (or (null? reduc) (single-tag? cont reduc)))
				(error "optimize: reduc" reduc expr))
			    fexp)))))
)

;;; For now:

(define (optimize/expression expr cont receiver)
  ((cond ((variable? expr) optimize/variable)
	 ((combination? expr) optimize/combination)
	 ((sequence? expr) optimize/sequence)
	 ((conditional? expr) optimize/conditional)
	 ((disjunction? expr) optimize/disjunction)
	 ((lambda? expr) optimize/lambda)
	 ((assignment? expr) optimize/assignment)
	 ((access? expr) optimize/access)
	 ((definition? expr) optimize/definition)
	 ((in-package? expr) optimize/in-package)
	 ((access? expr) optimize/access)
	 ((the-environment? expr) optimize/the-environment)
	 ((comment? expr) optimize/comment)
	 (else optimize/constant))
   expr
   cont
   receiver))

;;; This can be disabled after debugging

(define (check-reduc form expr set)
  (if (not (null? set))
      (error "optimize-imperative inconsistency: reductions in subproblem"
	     form expr set)))

;;;; Conditionals

(define (subproblem-sequence seq cont receiver make)
  (let ((acts (sequence-actions seq)))
    (optimize/expression
     (make-sequence
      (append (except-last-pair acts)
	      (list (make (car (last-pair acts))))))
     cont
     receiver)))

(define (optimize/conditional expr cont receiver)
  (conditional-components expr
    (lambda (opred ocons oalt)
      (if (sequence? opred)
	  (subproblem-sequence opred cont receiver
			       (lambda (last)
				 (make-conditional last ocons oalt)))
	  (optimize/subproblem
	   opred (make-subproblem-tag 'CONDITIONAL)
	   (lambda (fpred ipred psubpr preduc)
	     ipred			; ignored
	     (check-reduc opred expr preduc)
	     (optimize/expression
	      ocons cont
	      (lambda (fcons icons csubpr creduc)
		(optimize/expression
		 oalt cont
		 (lambda (falt ialt asubpr areduc)
		   (receiver
		    (make-conditional fpred fcons falt)
		    (make-conditional fpred icons ialt)
		    (add-tags! psubpr (add-tags! csubpr asubpr))
		    (add-tags! creduc areduc))))))))))))

(define (optimize/disjunction expr cont receiver)
  (disjunction-components expr
    (lambda (opred oalt)
      (if (sequence? opred)
	  (subproblem-sequence opred cont receiver
			       (lambda (last)
				 (make-disjunction last oalt)))
	  (let ((original (original-continuation cont)))
	    (case (tag-type original)
	      ((SUBPROBLEM RETURN)
	       (optimize/subproblem
		opred (make-subproblem-tag 'DISJUNCTION)
		(lambda (fpred ipred psubpr preduc)
		  ipred			; ignored
		  (check-reduc opred expr preduc)
		  (optimize/expression
		   oalt cont
		   (lambda (falt ialt asubpr areduc)
		     (receiver
		      (make-disjunction fpred falt)
		      (make-disjunction fpred ialt)
		      (add-tags! psubpr asubpr)
		      areduc))))))
	      (else
	       (optimize/conditional
		(make-conditional opred #t oalt)
		cont
		receiver))))))))

;;;; Sequences

(define (optimize/sequence expr cont receiver)
  (optimize/sequence-internal (basic-continuation cont)
			      (sequence-actions expr)
			      cont receiver))

(define (optimize/sequence-internal basic-cont forms cont receiver)
  (let loop ((forms forms)
	     (parsed '())
	     (subpr '()))
    (cond ((null? (cdr forms))
	   (optimize/expression
	    (car forms) cont
	    (lambda (fact iact nsubpr nreduc)
	      (receiver
	       (make-sequence (reverse (cons fact parsed)))
	       (make-sequence (reverse (cons iact parsed)))
	       (add-tags! nsubpr subpr)
	       nreduc))))
	  ((not basic-cont)
	   (optimize/subproblem
	    (car forms) (make-subproblem-tag 'SEQUENCE)
	    (lambda (fact iact nsubpr nreduc)
	      iact			; ignored
	      (check-reduc (car forms) forms nreduc)
	      (loop (cdr forms)
		    (cons fact parsed)
		    (add-tags! nsubpr subpr)))))
	  (else
	   (let ((tag (make-tagbody-tag (generate-uninterned-symbol)
					basic-cont)))
	     (optimize/expression
	      (car forms) tag
	      (lambda (fact iact nsubpr nreduc)
		(if (single-tag? tag nreduc)
		    (loop (cdr forms)
			  (cons fact parsed)
			  (add-tags! nsubpr subpr))
		    (optimize/sequence-internal
		     basic-cont (cdr forms) cont
		     (lambda (fnext inext xsubpr xreduc)
		       (let ((result
			      (make-tagbody-letrec
			       (list (tag-name tag))
			       (list (make-simple-lambda
				      '()
				      (if (and (single-tag? cont xreduc)
					       (not (sequence-tag? cont)))
					  fnext
					  inext)))
			       (make-sequence (reverse (cons iact parsed))))))
			 (receiver
			  result
			  result
			  (add-tags! xsubpr (add-tags! nsubpr subpr))
			  (add-tags! (singleton basic-cont)
				     (remove-tag tag
						 (add-tags! xreduc nreduc)))))))))))))))

(define (single-tag? tag reduc)
  (and (not (null? reduc))
       (null? (cdr reduc))
       (eq? (caar reduc) tag)))

(define (sequence-tag? tag)
  (let loop ((tags (cons tag (tag-synonyms tag))))
    (and (not (null? tags))
	 (case (tag-type (car tags))
	   ((SUBPROBLEM) false)
	   ((RETURN)
	    (loop (cdr tags)))
	   (else
	    (not (find-tag (tag-name (car tags)))))))))

'$split-file

;;;; Combinations

(define (optimize/combination expr cont receiver)
  (combination-components expr
    (lambda (operator operands)
      ((cond ((combination/return? operator operands) optimize/return)
	     ((combination/go? operator operands) optimize/go)
	     ((combination/tagbody? operator operands) optimize/tagbody)
	     ((combination/block? operator operands) optimize/block)
	     ((and (lambda? operator)
		   (not (eq? (tag-type cont) 'SUBPROBLEM)))
	      optimize/let)
	     ((and (variable? operator)
		   (iterate-tag? (variable-name operator)))
	      optimize/iterate-loop)
	     (else optimize/gcomb))
       expr operator operands cont receiver))))

(define (optimize/gcomb expr operator operands cont receiver)
  (optimize/subproblem
   operator (make-subproblem-tag 'OPERATOR)
   (lambda (fop iop osubpr oreduc)
     iop				; ignored
     (check-reduc operator expr oreduc)
     (optimize/operand-loop
      expr operands
      (lambda (arguments asubpr)
	(optimize/hairy
	 (make-combination fop arguments)
	 (add-tags! osubpr asubpr)
	 cont
	 receiver))))))
#|
(define (optimize/let expr operator operands cont receiver)
  ;; This assumes that all tags, etc are uninterned, so there can be no
  ;; conflict.  There is no need to alpha-rename.
  (lambda-components operator
    (lambda (name req opt rest aux decl body)
      (optimize/expression
       body cont
       (lambda (fbody ibody bsubpr breduc)
	 (optimize/operand-loop
	  expr
	  operands
	  (lambda (arguments asubpr)
	    (receiver
	     (make-combination
	      (external-make-lambda name req opt aux rest decl fbody)
	      arguments)
	     (make-combination
	      (external-make-lambda name req opt aux rest decl ibody)
	      arguments)
	     (add-tags! asubpr bsubpr)
	     breduc))))))))
|#

;; ***

(define (optimize/let expr operator operands cont receiver)
  ;; This assumes that all tags, etc are uninterned, so there can be no
  ;; conflict.  There is no need to alpha-rename.
  (lambda-components operator
    (lambda (name req opt rest aux decl body)
      (if (and aux (sequence? body))
	  (split-scanned-defines
	   aux
	   body
	   (lambda (defines actions)
	     (optimize/scanned-defines
	      defines
	      (lambda (ndefines nsubprs)
		(optimize/expression
		 actions cont
		 (lambda (fbody ibody bsubpr breduc)
		   (optimize/operand-loop
		    expr
		    operands
		    (lambda (arguments asubpr)
		      (rejoin-scanned-defines
		       ndefines fbody
		       (lambda (nfbody)
			 (rejoin-scanned-defines
			  ndefines ibody
			  (lambda (nibody)
			    (receiver
			     (make-combination
			      (external-make-lambda name req opt rest aux decl nfbody)
			      arguments)
			     (make-combination
			      (external-make-lambda name req opt rest aux decl nibody)
			      arguments)
			     (add-tags! nsubprs (add-tags! asubpr bsubpr))
			     breduc)))))))))))))
	  (optimize/expression
	   body cont
	   (lambda (fbody ibody bsubpr breduc)
	     (optimize/operand-loop
	      expr
	      operands
	      (lambda (arguments asubpr)
		(receiver
		 (make-combination
		  (external-make-lambda name req opt aux rest decl fbody)
		  arguments)
		 (make-combination
		  (external-make-lambda name req opt aux rest decl ibody)
		  arguments)
		 (add-tags! asubpr bsubpr)
		 breduc)))))))))

(define (optimize/operand-loop expr ops end)
  (let loop ((forms (reverse ops))
	     (parsed '())
	     (subpr '()))
    (if (null? forms)
	(end parsed subpr)
	(optimize/subproblem
	 (car forms) (make-subproblem-tag 'OPERAND)
	 (lambda (farg iarg asubpr areduc)
	   iarg				; ignored
	   (check-reduc (car forms) expr areduc)
	   (loop (cdr forms)
		 (cons farg parsed)
		 (add-tags! asubpr subpr)))))))

;;;; Simple loops

(define (optimize/iterate expr cont receiver)
  (iterate-components
   expr
   (lambda (iterate-name loop-name body-lam init-values)
     (lambda-components body-lam
       (lambda (name req opt rest aux decl body)
	 (with-renamed-continuation
	  loop-name cont 'ITERATE
	  (lambda (new subpr reduc)
	    (receiver (car new) (cdr new) subpr reduc))
	  (lambda (ncont recvr)
	    (optimize/expression
	     body ncont
	     (lambda (fbody ibody bsubpr breduc)
	       (optimize/operand-loop
		expr
		init-values
		(lambda (arguments asubpr)
		  (recvr
		   (cons
		    (make-final-iterate
		     iterate-name
		     loop-name
		     (external-make-lambda name req opt rest aux decl fbody)
		     arguments)
		    (make-final-iterate
		     iterate-name
		     loop-name
		     (external-make-lambda name req opt rest aux decl ibody)
		     arguments))
		   (add-tags! asubpr bsubpr)
		   breduc))))))))))))

(define (iterate-tag? name)
  (let ((tag (find-tag name)))
    (and tag
	 (eq? (tag-name tag) 'ITERATE))))

(define (optimize/iterate-loop expr operator operands cont receiver)
  (let ((tag (find-tag (variable-name operator))))
    (if (not (same-continuation? tag cont))
	(error "optimize/iterate-loop: Not the same continuation!" tag cont))
    (optimize/subproblem
     operator (make-subproblem-tag 'OPERATOR)
     (lambda (fop iop osubpr oreduc)
       iop				; ignored
       (check-reduc operator expr oreduc)
       (optimize/operand-loop
	expr operands
	(lambda (arguments asubpr)
	  (let ((expr (make-combination fop arguments)))
	    (receiver expr expr asubpr (singleton tag)))))))))

;;;; Leaf expressions

(define (optimize/trivial expr subpr cont receiver)
  (let ((original (original-continuation cont)))
    (case (tag-type original)
      ((SUBPROBLEM)
       (receiver expr expr subpr (singleton cont)))
      ((RETURN)
       (receiver expr expr subpr (singleton cont)))
      (else
       ;; trivial expression in effect position.  Punt.
       (receiver '()			; fall through
		 (make-optimized-go (tag-name original))
		 '()
		 (singleton original))))))

(define (optimize/hairy expr subpr cont receiver)
  (let ((original (original-continuation cont)))
    (case (tag-type cont)
      ((SUBPROBLEM)
       (receiver expr expr subpr (singleton cont)))
      ((RETURN)
       (receiver expr expr subpr (singleton cont)))
      (else
       (receiver expr
		 (make-sequence
		  (list expr (make-optimized-go (tag-name original))))
		 subpr
		 (singleton original))))))

(define (optimize/subproblem expr the-subpr-tag receiver)
  (optimize/expression 
   expr the-subpr-tag
   (lambda (fbody ibody subpr reduc)
     (let ((removed (remove-tag the-subpr-tag reduc)))
       (check-reduc expr expr removed)
       (if (element? the-subpr-tag subpr)
	   (error "subproblem inconsistency: optimize/subproblem" expr the-subpr-tag subpr))
       (receiver fbody
		 ibody
		 subpr
		 removed)))))

;;; Trivial forms

(define (optimize/constant expr cont receiver)
  (optimize/trivial expr '() cont receiver))

(define (optimize/variable expr cont receiver)
  (optimize/trivial (make-variable (variable-name expr))
		    '() cont receiver))

(define (optimize/the-environment expr cont receiver)
  (optimize/trivial expr '() cont receiver))

;; This assumes that the quotation does not share with
;; the surrounding code (ie will not be scode-evalled in the
;; surrounding code.).

(define (optimize/scode-quote expr cont receiver)
  (optimize/trivial expr '() cont receiver))

;;;; Simple special forms

#|
(define (optimize/lambda expr cont receiver)
  (lambda-components expr
    (lambda (name req opt rest aux decls body)
      (optimize/expression
       body (make-subproblem-tag 'LAMBDA)
       (lambda (fbody ibody subpr reduc)
	 ibody				; ignored
	 (check-reduc body expr reduc)
	 (optimize/trivial
	  (external-make-lambda name req opt rest aux decls fbody)
	  subpr cont receiver))))))
|#

(define (split-scanned-defines names sequence receiver)
  (sequence-components sequence
    (lambda (assignments+actions)
      (let loop ((names names)
		 (exprs assignments+actions)
		 (parsed-so-far '()))
	(if (null? names)
	    (receiver (make-sequence (reverse parsed-so-far)) (make-sequence exprs))
	    (loop (cdr names) (cdr exprs) (cons (car exprs) parsed-so-far)))))))

(define (rejoin-scanned-defines assignments actions receiver)
  (receiver (make-sequence (list assignments actions))))

;; ***
(define (optimize/scanned-defines assignments receiver)
  (let loop ((assignments (sequence-components assignments identity-procedure))
	     (parsed '())
	     (subpr '()))
    (if (null? assignments)
	(receiver (make-sequence (reverse parsed)) subpr)
	(optimize/subproblem 
	 (car assignments) 
	 (make-subproblem-tag 'SCANNED-DEFINES)
	 (lambda (fact iact nsubpr nreduc)
	   (check-reduc (car assignments) assignments nreduc)
	   (loop (cdr assignments) (cons fact parsed) (add-tags! nsubpr subpr)))))))

;; ***
(define (optimize/lambda expr cont receiver)
  (lambda-components expr
    (lambda (name req opt rest aux decls body)
      (if (and aux (sequence? body))
	  ;; Process the scanned out defines specially
	  (split-scanned-defines
	   aux
	   body
	   (lambda (defines actions)
	     (optimize/scanned-defines
	      defines
	      (lambda (ndefines nsubprs)
		(optimize/subproblem
		 actions (make-subproblem-tag 'LAMBDA)
		 (lambda (fbody ibody subpr reduc)
		   ibody			; ignored
		   (check-reduc body expr reduc)
		   (rejoin-scanned-defines
		    ndefines fbody
		    (lambda (nbody)
		      (optimize/trivial
		       (external-make-lambda name req opt rest aux decls nbody)
		       (add-tags subpr nsubprs) cont receiver)))))))))
	  (optimize/subproblem
	   body (make-subproblem-tag 'LAMBDA)
	   (lambda (fbody ibody subpr reduc)
	     ibody			; ignored
	     (check-reduc body expr reduc)
	     (optimize/trivial
	      (external-make-lambda name req opt rest aux decls fbody)
	      subpr cont receiver)))))))

(define (optimize/delay expr cont receiver)
  (delay-components expr
    (lambda (nexpr)
      (optimize/subproblem
       nexpr (make-subproblem-tag 'DELAY)
       (lambda (fexpr iexpr subpr reduc)
	 iexpr				; ignored
	 (check-reduc nexpr expr reduc)
	 (optimize/trivial (make-delay fexpr)
			   subpr cont receiver))))))

(define (optimize/comment expr cont receiver)
  (if (comment/iterate? expr)
      (optimize/iterate expr cont receiver)
      (comment-components expr
	(lambda (text expr)
	  (optimize/expression
	   expr cont
	   (lambda (fexpr iexpr subpr reduc)
	     (receiver (make-comment text fexpr)
		       (make-comment text iexpr)
		       subpr
		       reduc)))))))
      
#|
;; Missing:

open-block				; must be treated specially?
|#

'$split-file

;;;; Non-trivial special forms

(define ((make-envop-optimizer name envop-components make-envop)
	 expr cont receiver)
  (envop-components expr
    (lambda (name value)
      (optimize/subproblem
       value (make-subproblem-tag name)
       (lambda (fval ival subpr reduc)
	 ival				; ignored
	 (check-reduc value expr reduc)
	 (optimize/hairy (make-envop name fval)
			 subpr
			 cont
			 receiver))))))

(define optimize/assignment
  (make-envop-optimizer 'ASSIGNMENT
			assignment-components
			make-assignment))

(define optimize/definition
  (make-envop-optimizer 'DEFINITION
			definition-components
			make-definition))

(define optimize/access
  (make-envop-optimizer 'ACCESS
			(lambda (expr receiver)
			  (access-components expr
			    (lambda (environment name)
			      (receiver name environment))))
			(lambda (name environment)
			  (make-access environment name))))

(define (optimize/in-package expr cont receiver)
  (in-package-components expr
    (lambda (environment code)
      (optimize/subproblem
       environment (make-subproblem-tag 'IN-PACKAGE-ENV)
       (lambda (fenv ienv esubpr ereduc)
	 ienv				; ignored
	 (check-reduc environment expr ereduc)
	 ;; Wimping out...
	 (optimize/subproblem
	  code (make-subproblem-tag 'IN-PACKAGE-BODY)
	  (lambda (fcode icode bsubpr breduc)
	    icode			; ignored
	    (check-reduc code expr breduc)
	    (optimize/hairy (make-in-package fenv fcode)
			    (add-tags! esubpr bsubpr)
			    cont
			    receiver))))))))

;;;; GO and RETURN

(define (optimize/go expr operator operands cont receiver)
  expr					; ignored
  (go-components
   operator operands
   (lambda (tagbody-label label)
     (let ((tag (find-tag-fail label)))
       ;; The tag must be a user tag.
       (if (or (same-continuation? tag cont)
	       (can-optimize-go? tag cont))
	   (let ((expr (make-optimized-go label)))
	     (receiver expr expr '() (singleton tag)))	       
	   (let ((expr (make-go tagbody-label label)))
	     (receiver expr expr (singleton tag) '())))))))

(define (optimize/return expr operator operands cont receiver)
  expr					; ignored
  (return-components
   operator operands
   (lambda (label form)
     (let ((tag (find-tag-fail label)))
       (cond ((can-optimize-return? tag cont)
	      ;; *** Is cont ignored here? ***
	      (optimize/expression form tag receiver))
	     ((and (eq? (tag-type cont) 'SUBPROBLEM)
		   (eq? (tag-name cont) 'RETURN))
	      ;; This takes care of (return (return <mumble>))
	      (optimize/expression form cont receiver))
	     (else
	      (optimize/subproblem
	       form (make-subproblem-tag 'RETURN)
	       (lambda (fform iform fsubpr freduc)
		 iform			; ignored
		 (check-reduc form expr freduc)
		 (let ((expr (make-return label fform)))
		   (receiver expr expr
			     (add-tags! (singleton tag) fsubpr)
			     '()))))))))))

;;;; TAGBODY and BLOCK

;; block and tagbody do something funny to make fluid-let work
;; with cps code.

(define (optimize/block expr op args cont receiver)
  expr					; ignored
  (block-components
   op args
   (lambda (label body)
     (with-renamed-continuation
      label cont 'BLOCK
      (lambda (new subpr reduc)
	(receiver new new subpr reduc))
      (lambda (ncont recvr)
	(optimize/expression
	 body ncont
	 (lambda (fexpr iexpr subpr reduc)
	   (let ((nbody
		  (if (single-tag? ncont reduc)
		      fexpr
		      iexpr)))
	     (recvr (if (element? ncont subpr)
			(make-block label nbody)
			nbody)
		    subpr
		    reduc)))))))))	       

(define (optimize/tagbody expr op args cont receiver)
  expr					; ignored
  (tagbody-components
   op args
   (lambda (label preface segments)
     (with-renamed-continuation
      label cont 'TAGBODY
      (lambda (new subpr reduc)
	(receiver new new subpr reduc))
      (lambda (ncont recvr)
	(with-tagbody-tags
	 ncont (map car segments)
	 (lambda (tags)
	   (optimize/tagbody-internal
	    label preface segments tags
	    ncont recvr))))))))

;;;; Utilities for tagbody

(define (optimize/tagbody-internal label preface segments stags cont recvr)
  (let loop ((forms (reverse (cons (cons '() preface) segments)))
	     (parsed '())
	     (subpr '())
	     (reduc '()))
    (optimize/expression
     (cdar forms) cont
     (lambda (fseg iseg ssubpr sreduc)
       (let ((new (if (single-tag? cont sreduc)
		      fseg
		      iseg))
	     (nsubpr (add-tags! ssubpr subpr))
	     (nreduc (add-tags! sreduc reduc)))
	 (if (null? (cdr forms))
	     (remake-tagbody label new (reverse parsed) stags
			     nsubpr nreduc recvr)
	     (loop (cdr forms)
		   (cons (cons (caar forms) new)
			 parsed)
		   nsubpr
		   nreduc)))))))

(define (remake-tagbody label preface segments segtags subpr reduc recvr)
  (let ((names
	 (map car segments))
	(values
	 (map (lambda (segment)
		(make-simple-lambda '() (cdr segment)))
	      segments)))
    (let ((body (make-tagbody-letrec names values preface)))
      (recvr (if (any-in? segtags subpr)
		 (letrec->tagbody label body)
		 body)
	     subpr
	     reduc))))

;;;; Continuation manager

(define *continuations*)

(define top-level-tag (make-named-tag "TOP-LEVEL"))

(define (with-top-level-continuation handler)
  (let ((cont (make-return-tag 'TOP-LEVEL)))
    (fluid-let ((*continuations* `((,top-level-tag . ,cont))))
      (handler cont))))

(define (with-renamed-continuation label cont kind after handler)
  (let ((new (make-synonym-tag kind cont)))
    (apply after
	   (let ((old *continuations*))
	     (fluid-let ((*continuations*
			  `((,label . ,new) ,@*continuations*)))
	       (handler new
			(lambda (new nsubpr nreduc)
			  (list new
				(remove-not old nsubpr)
				(remove-not old nreduc)))))))))

(define (with-tagbody-tags cont labels handler)
  (let ((new-tags (map (lambda (label)
			 (make-tagbody-tag label cont))
		       labels)))
    (fluid-let ((*continuations*
		 (map* *continuations* cons labels new-tags)))
      (handler new-tags))))

(define (add-tagbody-tag! tag code)
  (let ((place (assq label *continuations*)))
    (if (null? place)
	(error "find-tag: Unknown label/return point" label)
	(cdr place))))

(define (find-tag label)
  (let ((place (assq label *continuations*)))
    (if (null? place)
	false
	(cdr place))))

(define (find-tag-fail label)
  (let ((place (assq label *continuations*)))
    (if (null? place)
	(error "find-tag-fail: Unknown tag" label)
	(cdr place))))
  

'$split-file

;;;; Continuation abstraction

(define tag-type car)
(define tag-name cadr)
(define tag-synonyms cddr)

(define (make-tag kind name synonyms)
  (cons* kind name synonyms))

(define (make-synonym-tag name base)
  (make-tag 'RETURN name (cons base (tag-synonyms base))))

(define (make-return-tag name)
  (make-tag 'RETURN name '()))

(define (make-subproblem-tag name)
  (make-tag 'SUBPROBLEM name '()))

(define (make-tagbody-tag label tag)
  (make-tag tag label '()))

(define (original-continuation cont)
  (let ((syns (tag-synonyms cont)))
    (if (null? syns)
	cont
	(car (last-pair syns)))))

#|
;;; This seems more correct.

(define (basic-continuation cont)
  (let ((original (original-continuation cont)))
    (case (tag-type original)
      ((SUBPROBLEM) '())
      ((RETURN) original)
      (else
       (tag-type original)))))
|#

(define (basic-continuation tag)
  (let loop ((syns (cons tag (tag-synonyms tag))))
    (and (not (null? syns))
	 (case (tag-type (car syns))
	   ((SUBPROBLEM)
	    (loop (cdr syns)))
	   ((RETURN)
	    ;; *** Irrelevant of whether it is TAGBODY, BLOCK, or TOP-LEVEL ***
	    (car syns))
	   (else
	    (tag-type (car syns)))))))

(define (same-continuation? tag1 tag2)
  (or (eq? tag1 tag2)
      (memq tag1 (tag-synonyms tag2))
      (memq tag2 (tag-synonyms tag1))))

(define (can-optimize-go? target cont)
  ;; target IS a tagbody tag
  (let ((tagbody (tag-type target)))
    (let loop ((syns (cons cont (tag-synonyms cont))))
      (and (not (null? syns))
	   (or (eq? tagbody (car syns))
	       (eq? tagbody (tag-type (car syns)))
	       (loop (cdr syns)))))))

#|
;; Not quite enough

(define (can-optimize-return? target cont)
  (let loop ((syns (cons cont (tag-synonyms cont))))
    (and (not (null? syns))
	 (or (and (not (memq (tag-type (car syns)) '(RETURN SUBPROBLEM)))
		  (same-continuation? target (tag-type (car syns))))
	     (loop (cdr syns))))))
|#

(define (can-optimize-return? target cont)
  (let loop ((to-check (list cont)))
    (and (not (null? to-check))
	 (let* ((this (car to-check))
		(syns (tag-synonyms this)))
	   (or (eq? target this)
	       (memq this (tag-synonyms target))
	       (loop (if (not (memq (tag-type this) '(RETURN SUBPROBLEM)))
			 (append syns (cons (tag-type this) (cdr to-check)))
			 (append syns (cdr to-check)))))))))

;;;; Tag set abstraction

(define (add-elements! s1 s2)
  (if (null? s1)
      s2
      (let ((place (assq (caar s1) s2)))
	(if (null? place)
	    (add-elements! (cdr s1)
			   (cons (car s1) s2))
	    (begin
	      (set-car! (cdr place)
			(+ (cadar s1) (cadr place)))
	      (add-elements! (cdr s1) s2))))))

(define (add-tags! s1 s2)
  (cond ((null? s1) s2)
	((null? s2) s1)
	((< (length s1) (length s2))
	 (add-elements! s1 s2))
	(else
	 (add-elements! s2 s1))))

(define (remove-tag tag set)
  (del-assq! tag set))

(define (singleton tag)
  (list (list tag 1)))

(define (element? object set)
  (assq object set))

(define (any-in? objs set)
  (and (not (null? objs))
       (or (element? (car objs) set)
	   (any-in? (cdr objs) set))))

(define (remove-not allowed all)
  (let outer ((to-test all)
	      (tested '()))
    (if (null? to-test)
	tested
	(let ((this-tag (caar to-test)))
	  (let inner ((not-checked allowed))
	    (cond ((null? not-checked)
		   (outer (cdr to-test) tested))
		  ((eq? this-tag (cdar not-checked))
		   (outer (cdr to-test)
			  (cons (car to-test) tested)))
		  (else
		   (inner (cdr not-checked)))))))))

;;;; Syntax

(define (combination/go? op args)
  args					; ignored
  (and (variable? op)
       (let ((tag (find-tag (variable-name op))))
	 (if tag
	     (case (tag-type tag)
	       ((SUBPROBLEM) false)
	       ((RETURN)
		(eq? 'TAGBODY (tag-name tag)))
	       (else true))
	     (let ((place (assq (variable-name op) *continuations*)))
	       (and place
		    (let ((tag (cdr place)))
		      (and (eq? 'RETURN (tag-type tag))
			   (eq? 'TAGBODY (tag-name tag))))))))))

(define (go-components op args recvr)
  (if (null? args)			; simple go?
      (let ((label (variable-name op)))
	(recvr (tag-name (tag-type (find-tag-fail label)))
	       label))
      (recvr (variable-name op)
	     (variable-name (car args)))))

(define (make-optimized-go label)
  (make-combination (make-variable label) '()))

(define (make-go tag-label label)
  (make-combination (make-variable tag-label)
		    (list (make-variable label))))

(define (combination/return? op args)
  (define (check var)
    (let ((place (assq (variable-name var) *continuations*)))
      (and place
	   (let ((tag (cdr place)))
	     (and (eq? 'RETURN (tag-type tag))
		  (eq? 'BLOCK (tag-name tag)))))))

  (if (variable? op)
      (check op)
      (and (eq? prim-with-values op)
	   (variable? (cadr args))
	   (check (cadr args)))))

(define (return-components op args recvr)
  (if (variable? op)
      (recvr (variable-name op) (car args))
      (recvr (variable-name (cadr args))
	     (lambda-body (car args)))))

;;;; More syntax

(define (combination/block? op args)
  (and (absolute-variable? op)
       (eq? (absolute-variable-name op) '%BLOCK-AUX%)))

(define (block-components op args recvr)
  (lambda-components (car args)
    (lambda (name req opt rest aux decls body)
      name opt rest aux decls		; ignored
      (recvr (car req) body))))

(define (combination/tagbody? op args)
  (if (absolute-variable? op)
      (eq? (absolute-variable-name op) '%TAGBODY-AUX%)
      ;; Only needed to make this code reentrant
      (and (lambda? op)
	   (lambda-components op
	     (lambda (name req opt rest aux decls body)
	       req opt rest aux decls body ; ignored
	       (eq? name lambda-tag:tagbody-letrec))))))
	   
(define (tagbody-components op args recvr)
  (if (absolute-variable? op)
      (lambda-components (car args)
	(lambda (name req opt rest aux decls body)
	  name opt rest aux decls	; ignored
	  (letrec/tagbody-components (car req) body recvr)))
      (letrec/tagbody-components
       (generate-uninterned-symbol)
       (make-combination op args)
       recvr)))

(define (letrec/tagbody-components label expr recvr)
  (combination-components expr
    (lambda (op args)
      args				; ignored
      (lambda-components op
	(lambda (name req opt rest aux decls body)
	  name req opt rest decls	; ignored
	  (let process ((acts (sequence-actions body))
			(names aux)
			(segments '()))
	    (cond ((null? acts)
		   (error "letrec/tagbody-components: bad letrec" expr))
		  ((null? names)
		   (recvr label
			  (make-sequence acts)
			  (reverse segments)))
		  (else
		   (assignment-components (car acts)
		     (lambda (name value)
		       (if (not (eq? name (car names)))
			   (error "letrec/tagbody-components: bad letrec" expr))
		       (process (cdr acts)
				(cdr names)
				(cons (cons name (lambda-body value))
				      segments))))))))))))
