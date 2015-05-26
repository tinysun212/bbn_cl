#| -*-Scheme-*-

<<<<<<< Xterm.scm
$Header: Xterm.scm,v 13.90 88/06/06 08:59:51 ajc Exp $
=======
$Header: Xterm.scm,v 13.90 88/06/06 08:59:51 ajc Exp $
>>>>>>> 13.80.2.1

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

;;;; X Window Terminal

(declare (usual-integrations))

(define xterm-package
  (make-environment

(define (xterm-open program-name geometry font-name)
  (let ((internal-border-width 3)
	(default-geometry "=80x24+1+1"))
    (let ((adder (* internal-border-width 2)))
      (let ((frame
	     (x-make-opaque-frame 0 0 0 0 4 (x-black-pixmap) (x-white-pixmap)))
	    (font-info
	     (x-query-font (or (x-get-font font-name)
			       (error "Unable to open font" font-name)))))
	(if (not (x-font-info->fixedwidth font-info))
	    (error "Font is not fixed width" font-name))
	(let ((xterm
	       (let ((results
		      (x-create-term "Scheme Window" program-name
				     (or geometry default-geometry)
				     default-geometry
				     frame 10 5 adder adder
				     font-info
				     (x-font-info->width font-info)
				     (x-font-info->height font-info))))
		 (xterm/make (vector-ref results 0)
			     (vector-ref results 1)
			     (vector-ref results 2)
			     font-info
			     internal-border-width
			     pixel-value/white
			     pixel-value/black))))
	  (x-select-input (xterm/window xterm)
			  (+ event-type/key-pressed
			     event-type/expose-window
			     event-type/expose-region
			     event-type/unmap-window))
	  (xterm/visible! xterm)
	  (x-map-window (xterm/window xterm))
	  xterm)))))

(define (xterm-close xterm)
  (without-interrupts			;implies with-input-blocked! as well.
   (lambda ()
     (let ((window (xterm/window xterm)))
       (x-destroy-window window)
       (let loop ()
	 (if (x-check-window-event window event-mask/all)
	     (loop))))
     (set! xterm-list (delq! xterm xterm-list))
     (set! expose-window-queue (delq! xterm expose-window-queue))
     (set! expose-region-queue (del-assq! xterm expose-region-queue)))))

(define (xterm-map xterm)
  (with-input-blocked!
   (lambda ()
     (if (not (xterm/visible? xterm))
	 (begin
	   (xterm/visible! xterm)
	   (x-map-window (xterm/window xterm)))))))

(define (xterm-unmap xterm)
  (with-input-blocked!
   (lambda ()
     (if (xterm/visible? xterm)
	 (begin
	   (xterm/invisible! xterm)
	   (x-unmap-window (xterm/window xterm)))))))

(define (xterm-write-string xterm x y string)
  (xterm-write-substring xterm x y string 0 (string-length string)))

(define (xterm-write-substring xterm x y string start end)
  (with-input-blocked!
   (lambda ()
     (substring-move-right! string start end
			    (vector-ref (xterm/char-map xterm) y) x)
     (if (xterm/visible? xterm)
	 (let ((border (xterm/internal-border-width xterm))
	       (font-info (xterm/font-info xterm)))
	   (x-text (xterm/window xterm)
		   (+ border (* x (x-font-info->width font-info)))
		   (+ border (* y (x-font-info->height font-info)))
		   (substring string start end)
		   (x-font-info->id font-info)
		   (xterm/foreground xterm)
		   (xterm/background xterm)))))))

(define (xterm-write-char xterm x y char)
  (with-input-blocked!
   (lambda ()
     (string-set! (vector-ref (xterm/char-map xterm) y) x char)
     (if (xterm/visible? xterm)
	 (let ((border (xterm/internal-border-width xterm))
	       (font-info (xterm/font-info xterm)))
	   (x-text (xterm/window xterm)
		   (+ border (* x (x-font-info->width font-info)))
		   (+ border (* y (x-font-info->height font-info)))
		   (char->string char)
		   (x-font-info->id font-info)
		   (xterm/foreground xterm)
		   (xterm/background xterm)))))))

(define (xterm-inverse-video xterm)
  (xterm/set-foreground! xterm pixel-value/white)
  (xterm/set-background! xterm pixel-value/black))

(define (xterm-normal-video xterm)
  (xterm/set-foreground! xterm pixel-value/black)
  (xterm/set-background! xterm pixel-value/white))

(define (xterm-clear xterm)
  (with-input-blocked!
   (lambda ()
     (let ((char-map (xterm/char-map xterm))
	   (y-end (xterm/y-size xterm)))
       (let loop ((y 0))
	 (if (< y y-end)
	     (begin
	       (string-fill! (vector-ref char-map y) #\Space)
	       (loop (1+ y))))))
     (if (xterm/visible? xterm)
	 (x-clear (xterm/window xterm))))))

(define (xterm-clear-subscreen xterm x-start x-end y-start y-end)
  (with-input-blocked!
   (lambda ()
     (if (< x-start x-end)
	 (begin
	   (let ((char-map (xterm/char-map xterm)))
	     (let loop ((y y-start))
	       (if (< y y-end)
		   (begin
		     (substring-fill! (vector-ref char-map y) x-start x-end
				      #\Space)
		     (loop (1+ y))))))
	   (if (xterm/visible? xterm)
	       (let ((border (xterm/internal-border-width xterm))
		     (font-info (xterm/font-info xterm)))
		 (let ((fwidth (x-font-info->width font-info))
		       (fheight (x-font-info->height font-info)))
		   (x-pix-set (xterm/window xterm)
			      (+ border (* x-start fwidth))
			      (+ border (* y-start fheight))
			      (* (- x-end x-start) fwidth)
			      (* (- y-end y-start) fheight)
			      pixel-value/white)))))))))

;;;; Event Processing

(define event-processing-interval -50)	;half-second real-time interval
(define old-timer-interrupt false)
(define interrupt-mask/gc+character (+ interrupt-mask-gc-ok #x40))

(define (with-input-blocked! thunk)
  (with-interrupt-mask interrupt-mask/gc+character
   (lambda (old-mask)
     (thunk))))

(define (event-processing-enabled?)
  old-timer-interrupt)

(define (event-processing-enable!)
  (if (not old-timer-interrupt)
      (without-interrupts
       (lambda ()
	 (set! old-timer-interrupt timer-interrupt)
	 (set! timer-interrupt event-processing-interrupt)
	 (setup-timer-interrupt 0 event-processing-interval)))))

(define (event-processing-disable!)
  (if old-timer-interrupt
      (without-interrupts
       (lambda ()
	 (setup-timer-interrupt false false)
	 (set! timer-interrupt old-timer-interrupt)
	 (set! old-timer-interrupt false)))))

(define (event-processing-interrupt)
  (set-interrupt-enables! interrupt-mask-gc-ok)
  (dynamic-wind (lambda ()
		  (setup-timer-interrupt false false))
		process-events
		(lambda ()
		  (setup-timer-interrupt 0 event-processing-interval))))
     
(define setup-timer-interrupt
  (make-primitive-procedure 'SETUP-TIMER-INTERRUPT))

(define (process-events)
  (let loop ()
    (if (positive? (x-pending))
	(begin (process-event (x-next-event))
	       (loop))))
  (expose-windows)
  (expose-regions))

(define (process-event event)
  (let ((xterm (window->xterm (x-event-window event)))
	(type (x-event-type event)))
    (cond ((= type event-type/expose-window)
	   (process-event/expose-window xterm))
	  ((= type event-type/expose-region)
	   (process-event/expose-region xterm event))
	  ((= type event-type/unmap-window)
	   (xterm/invisible! xterm))
	  ((= type event-type/key-pressed)
	   (process-event/key-pressed xterm event))
	  (else
	   (error "Unsolicited event" event)))))

(define (expose-windows)
  (if (not (null? expose-window-queue))
      (begin (let ((xterm (car expose-window-queue)))
	       (set! expose-window-queue (cdr expose-window-queue))
	       (expose-window xterm))
	     (expose-windows))))

(define (expose-regions)
  (if (not (null? expose-region-queue))
      (begin (let ((element (car expose-region-queue)))
	       (set! expose-region-queue (cdr expose-region-queue))
	       (expose-region (car element) (cdr element)))
	     (expose-regions))))

(define expose-window-queue '())
(define expose-region-queue '())

(define (process-event/expose-window xterm)
  (if (not (memq xterm expose-window-queue))
      (begin (set! expose-window-queue
		   (append! expose-window-queue (list xterm)))
	     (set! expose-region-queue
		   (del-assq! xterm expose-region-queue))
	     (xterm/visible! xterm))))

(define (process-event/expose-region xterm event)
  (if (not (memq xterm expose-window-queue))
      (set! expose-region-queue
	    (append! expose-region-queue (list (cons xterm event))))))

(define (process-event/key-pressed xterm event)
  ((or (xterm/input-filter xterm) xterm/enqueue-input!)
   xterm
   (x-lookup-mapping event)))

;;;; Window Drawing

(define (expose-window xterm)
  (let ((window-info (x-query-window (xterm/window xterm)))
	(font-info (xterm/font-info xterm))
	(adder (* 2 (xterm/internal-border-width xterm))))
    (let ((width
	   (quotient (- (x-window-info->width window-info) adder)
		     (x-font-info->width font-info)))
	  (height
	   (quotient (- (x-window-info->height window-info) adder)
		     (x-font-info->height font-info))))
      (if (not (and (= width (xterm/x-size xterm))
		    (= height (xterm/y-size xterm))))
	  (xterm/set-size! xterm width height)))
    (dump-rectangle! xterm 0 (xterm/x-size xterm) 0 (xterm/y-size xterm))))

(define (expose-region xterm event)
  (let ((border (xterm/internal-border-width xterm))
	(font-info (xterm/font-info xterm)))
    (let ((x-start (- (x-expose-event-x event) border))
	  (y-start (- (x-expose-event-y event) border))
	  (font-width (x-font-info->width font-info))
	  (font-height (x-font-info->height font-info)))
      (let ((x-end (+ x-start (x-expose-event-width event)))
	    (y-end (+ y-start (x-expose-event-height event))))
	(let ((x-start* (max 0 (quotient-floor x-start font-width)))
	      (x-end*
	       (min (xterm/x-size xterm)
		    (quotient-ceiling x-end font-width)))
	      (y-start* (max 0 (quotient-floor y-start font-height)))
	      (y-end*
	       (min (xterm/y-size xterm)
		    (quotient-ceiling y-end font-height))))
	  (if (and (< x-start* x-end*) (< y-start* y-end*))
	      (dump-rectangle! xterm x-start* x-end* y-start* y-end*)))))))

(define (quotient-floor numerator denominator)
  (let ((qr (integer-divide numerator denominator)))
    (if (negative? (integer-divide-remainder qr))
	(-1+ (integer-divide-quotient qr))
	(integer-divide-quotient qr))))

(define (quotient-ceiling numerator denominator)
  (let ((qr (integer-divide numerator denominator)))
    (if (positive? (integer-divide-remainder qr))
	(1+ (integer-divide-quotient qr))
	(integer-divide-quotient qr))))

(define (dump-rectangle! xterm x-start x-end y-start y-end)
  (let ((window (xterm/window xterm))
	(offset (xterm/internal-border-width xterm))
	(font-info (xterm/font-info xterm))
	(char-map (xterm/char-map xterm)))
    (let ((x (+ offset (* x-start (x-font-info->width font-info))))
	  (y-increment (x-font-info->height font-info))
	  (font (x-font-info->id font-info)))
      (let loop
	  ((y-index y-start)
	   (y (+ offset (* y-start y-increment))))
	(if (< y-index y-end)
	    (begin (x-text window x y
			   (substring (vector-ref char-map y-index)
				      x-start
				      x-end)
			   font 0 1)
		   (loop (1+ y-index) (+ y y-increment))))))))

(define (xterm/set-size! xterm new-x-size new-y-size)
  (let ((x-limit (min (xterm/x-size xterm) new-x-size))
	(y-limit (min (xterm/y-size xterm) new-y-size))
	(old-char-map (xterm/char-map xterm))
	(new-char-map (make-char-map new-x-size new-y-size)))
    (let y-loop ((y 0))
      (if (< y y-limit)
	  (begin
	    (substring-move-right! (vector-ref old-char-map y) 0 x-limit
				   (vector-ref new-char-map y) 0)
	    (y-loop (1+ y)))))))

;;;; Input

(define (xterm->input-port xterm)
  (make-environment
    (define :type input-port-tag)

    (define (:print-self)
      (unparse-with-brackets
       (lambda ()
	 (write-string "Input port for ")
	 (write xterm))))

    (define (:char-ready? delay)
      (with-input-blocked
       (lambda ()
	 ;; Need to implement DELAY feature, which requires
	 ;; real-time clock primitive.
	 (not (null? (xterm/input-queue xterm))))))

    (define (:close)
      'DONE)

    (define (:peek-char)
      (wait-for-input xterm
	(lambda ()
	  (car (xterm/input-queue xterm)))))

    (define (:discard-char)
      (wait-for-input xterm
	(lambda ()
	  (xterm/set-input-queue! xterm (cdr (xterm/input-queue xterm))))))

    (define (:read-char)
      (wait-for-input xterm
	(lambda ()
	  (let ((char (car (xterm/input-queue xterm))))
	    (xterm/set-input-queue! xterm (cdr (xterm/input-queue xterm)))
	    char))))

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
      (loop))))

;;;; Low Level Stuff

(define (window->xterm window)
  (let loop ((xterms xterm-list))
    (cond ((null? xterms)
	   (error "Window not associated with known xterm" window))
	  ((x-object=? window (xterm/window (car xterms))) (car xterms))
	  (else (loop (cdr xterms))))))

(define (make-char-map x-size y-size)
  (make-initialized-vector y-size
    (lambda (y)
      (make-string x-size #\Space))))

(define (xterm/enqueue-input! xterm string)
  ;; Assumes input blocked.
  (xterm/set-input-queue! xterm
			  (append! (xterm/input-queue xterm)
				   (string->list string))))

(define (wait-for-input xterm thunk)
  (with-input-blocked!
   (lambda ()
     (let loop ()
       (if (null? (xterm/input-queue xterm))
	   (begin (process-events)
		  (loop))
	   (thunk))))))

(define xterm-list '())

(define (xterm/make window x-size y-size font-info internal-border-width
		    background foreground)
  (let ((xterm
	 (vector xterm/tag window x-size y-size font-info internal-border-width
		 background foreground false (make-char-map x-size y-size)
		 '() false false)))
    (vector-set! xterm 11 (xterm->input-port xterm))
    (set! xterm-list (append! xterm-list (list xterm)))
    xterm))

(define xterm/tag "XTerm")

((access add-unparser-special-object! unparser-package) xterm/tag
 (lambda (xterm)
   ((access unparse-with-brackets unparser-package)
    (lambda ()
      (write-string "X Terminal ")
      (write (hash xterm))))))

(let-syntax
    ((define-integrable
       (macro (pattern . body)
	 `(BEGIN (DECLARE (INTEGRATE-OPERATOR ,(car pattern)))
		 (DEFINE ,pattern
		   (DECLARE (INTEGRATE ,@(cdr pattern)))
		   ,@body)))))

(define-integrable (xterm/window xterm)
  (vector-ref xterm 1))

(define-integrable (xterm/x-size xterm)
  (vector-ref xterm 2))

(define-integrable (xterm/set-x-size! xterm x-size)
  (vector-set! xterm 2 x-size))

(define-integrable (xterm/y-size xterm)
  (vector-ref xterm 3))

(define-integrable (xterm/set-y-size! xterm y-size)
  (vector-set! xterm 3 y-size))

(define-integrable (xterm/font-info xterm)
  (vector-ref xterm 4))

(define-integrable (xterm/internal-border-width xterm)
  (vector-ref xterm 5))

(define-integrable (xterm/background xterm)
  (vector-ref xterm 6))

(define-integrable (xterm/set-background! xterm background)
  (vector-set! xterm 6 background))

(define-integrable (xterm/foreground xterm)
  (vector-ref xterm 7))

(define-integrable (xterm/set-foreground! xterm foreground)
  (vector-set! xterm 7 foreground))

(define-integrable (xterm/visible? xterm)
  (vector-ref xterm 8))

(define-integrable (xterm/invisible! xterm)
  (vector-set! xterm 8 false))

(define-integrable (xterm/visible! xterm)
  (vector-set! xterm 8 true))

(define-integrable (xterm/char-map xterm)
  (vector-ref xterm 9))

(define-integrable (xterm/input-queue xterm)
  (vector-ref xterm 10))

(define-integrable (xterm/set-input-queue! xterm queue)
  (vector-set! xterm 10 queue))

(define-integrable (xterm/input-port xterm)
  (vector-ref xterm 11))

(define-integrable (xterm/input-filter xterm)
  (vector-ref xterm 12))

(define-integrable (xterm/set-input-filter! xterm input-filter)
  (vector-set! xterm 12 input-filter))

;;; end LET-SYNTAX
)

;;; end XTERM-PACKAGE
))

(define xterm-open (access xterm-open xterm-package))
(define xterm-close (access xterm-close xterm-package))
(define xterm-map (access xterm-map xterm-package))
(define xterm-unmap (access xterm-unmap xterm-package))
(define xterm-write-string (access xterm-write-string xterm-package))
(define xterm-write-substring (access xterm-write-substring xterm-package))
(define xterm-write-char (access xterm-write-char xterm-package))
(define xterm-inverse-video (access xterm-inverse-video xterm-package))
(define xterm-normal-video (access xterm-normal-video xterm-package))
(define xterm-clear (access xterm-clear xterm-package))
(define xterm-clear-subscreen (access xterm-clear-subscreen xterm-package))
(define xterm/x-size (access xterm/x-size xterm-package))
(define xterm/y-size (access xterm/y-size xterm-package))
