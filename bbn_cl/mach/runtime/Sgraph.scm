;;; -*-Scheme-*-
;;;
<<<<<<< Sgraph.scm
<<<<<<< Sgraph.scm
;;;	$Header: Sgraph.scm,v 13.90 88/06/06 08:59:44 ajc Exp $
=======
;;;	$Header: Sgraph.scm,v 13.90 88/06/06 08:59:44 ajc Exp $
>>>>>>> 13.80.2.1
=======
;;;	$Header: Sgraph.scm,v 13.90 88/06/06 08:59:44 ajc Exp $
>>>>>>> 13.80.2.1
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

;;;; Student graphics Interface

(declare (usual-integrations))

(define graphics-available?)
(define init-graphics)
(define clear-graphics)
(define draw-line-to)
(define position-pen)
(define draw-point)
(define clear-point)
(define graphics-text)

(define graphics-package
  (make-environment

(define graphics-initialize
  (make-primitive-procedure 'GRAPHICS-INITIALIZE 0))

(define graphics-move
  (make-primitive-procedure 'GRAPHICS-MOVE 2))

(define graphics-line
  (make-primitive-procedure 'GRAPHICS-LINE 2))

(define graphics-clear
  (make-primitive-procedure 'GRAPHICS-CLEAR 0))

(define graphics-pixel
  (make-primitive-procedure 'GRAPHICS-PIXEL 2))

(define graphics-set-drawing-mode
  (make-primitive-procedure 'GRAPHICS-SET-DRAWING-MODE 1))

(define graphics-set-line-style
  (make-primitive-procedure 'GRAPHICS-SET-LINE-STYLE 1))

(define graphics-label
  (make-primitive-procedure 'GRAPHICS-LABEL 1))

(define graphics-set-letter
  (make-primitive-procedure 'GRAPHICS-SET-LETTER 3))

(define graphics-set-rotation
  (make-primitive-procedure 'GRAPHICS-SET-ROTATION 1))

(define drawing-mode:dominant 3)
(define drawing-mode:non-dominant 1)
(define drawing-mode:erase 0)
(define drawing-mode:complement 2)

(define *graphics-drawing-mode*
  drawing-mode:dominant)

(define *graphics-linestyle*
  0)

;;;; Student Graphics Interface

(set! graphics-available?
      (lambda ()
	(implemented-primitive-procedure? graphics-line)))

(set! init-graphics
      (lambda ()
	(graphics-initialize)
	(graphics-set-drawing-mode drawing-mode:dominant)
	(set! *graphics-drawing-mode* drawing-mode:dominant)
	(set-line-style 0)))

(set! clear-graphics
      (lambda ()
	(graphics-clear)
	(graphics-move 0 0)))

(set! draw-line-to
      (lambda (x y)
	(graphics-line (round x) (round y))))

(set! position-pen
      (lambda (x y)
	(graphics-move (round x) (round y))))

(define ((graphics-point-procedure drawing-mode) x y)
  (dynamic-wind (lambda ()
		  (graphics-set-drawing-mode drawing-mode))
	        (lambda ()
		  (graphics-pixel (round x) (round y)))
		(lambda ()
		  (graphics-set-drawing-mode *graphics-drawing-mode*))))

(set! draw-point (graphics-point-procedure drawing-mode:dominant))
(set! clear-point (graphics-point-procedure drawing-mode:erase))

;;;; Extra operations

(define (set-line-style style)
  (graphics-set-line-style style)
  (set! *graphics-linestyle* style))

(define (draw-line x y #!optional style)
  (if (unassigned? style)
      (graphics-line (round x) (round y))
      (dynamic-wind (lambda ()
		      (graphics-set-line-style style))
		    (lambda ()
		      (graphics-line (round x) (round y)))
		    (lambda ()
		      (graphics-set-line-style *graphics-linestyle*)))))

(define (erase-line x y)
  (dynamic-wind (lambda ()
		  (graphics-set-drawing-mode drawing-mode:erase)
		  (graphics-set-line-style 1))
                (lambda ()
		  (graphics-line (round x) (round y)))
                (lambda ()
		  (graphics-set-line-style *graphics-linestyle*)
		  (graphics-set-drawing-mode *graphics-drawing-mode*))))

(define (draw-pixel x y #!optional drawing-mode)
  (if (unassigned? drawing-mode)
      (graphics-pixel (round x) (round y))
      (dynamic-wind (lambda ()
		      (graphics-set-drawing-mode drawing-mode))
		    (lambda ()
		      (graphics-pixel (round x) (round y)))
		    (lambda ()
		      (graphics-set-drawing-mode *graphics-drawing-mode*)))))

;;;; Character Stuff

(define *graphics-character-slant* 0)
(define *graphics-character-rotation* 0)
(define *graphics-character-size* 50)
(define *graphics-character-aspect* 0.7)

(set! graphics-text
(lambda (text #!optional size rotation aspect slant)
  (if (unassigned? size)
      (set! size *graphics-character-size*))
  (if (unassigned? aspect)
      (set! aspect *graphics-character-aspect*))
  (if (unassigned? slant)
      (set! slant *graphics-character-slant*))
  (if (unassigned? rotation)
      (set! rotation *graphics-character-rotation*))
  (with-character-attributes size rotation aspect slant
    (lambda ()
      (graphics-label text)))))

(define (set-default-character-attributes!)
  (set! *graphics-character-slant* 0)
  (set! *graphics-character-rotation* 0)
  (set! *graphics-character-size* 50)
  (set! *graphics-character-aspect* 0.7)
  (reset-character-attributes!))

(define (reset-character-attributes!)
  (graphics-set-letter *graphics-character-size*
		       *graphics-character-aspect*
		       *graphics-character-slant*)
  (graphics-set-rotation *graphics-character-rotation*))

(define (set-character-attributes! #!optional size rotation aspect slant)
  (if (not (unassigned? size))
      (set! *graphics-character-size* size))
  (if (not (unassigned? aspect))
      (set! *graphics-character-aspect* aspect))
  (if (not (unassigned? slant))
      (set! *graphics-character-slant* slant))
  (if (not (unassigned? rotation))
      (set! *graphics-character-rotation* rotation))
  true)

(define (with-character-attributes size rotation aspect slant thunk)
  (let ((old-size) (old-rot) (old-aspect) (old-slant))
    (dynamic-wind
     (lambda ()
       (set! old-size (set! *graphics-character-size* size))
       (set! old-rot (set! *graphics-character-rotation* rotation))
       (set! old-aspect (set! *graphics-character-aspect* aspect))
       (set! old-slant (set! *graphics-character-slant* slant))
       (graphics-set-letter *graphics-character-size*
			    *graphics-character-aspect*
			    *graphics-character-slant*)
       (graphics-set-rotation *graphics-character-rotation*))
     thunk
     (lambda ()
       (set! *graphics-character-size* (set! old-size size))
       (set! *graphics-character-rotation* (set! old-rot rotation))
       (set! *graphics-character-aspect* (set! old-aspect aspect))
       (set! *graphics-character-slant* (set! old-slant slant))
       (graphics-set-letter *graphics-character-size* 
			    *graphics-character-aspect*
			    *graphics-character-slant*)
       (graphics-set-rotation *graphics-character-rotation*)))))

;;; end GRAPHICS-PACKAGE
))
