;;; -*-Scheme-*-
;;;
<<<<<<< Xgraph.scm
;;;	$Header: Xgraph.scm,v 13.90 88/06/06 08:59:46 ajc Exp $
=======
;;;	$Header: Xgraph.scm,v 13.90 88/06/06 08:59:46 ajc Exp $
>>>>>>> 13.80.2.1
;;;
;;;     Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;     This material was developed by the Scheme project at the
;;;     Massachusetts Institute of Technology, Department of
;;;     Electrical Engineering and Computer Science.  Permission to
;;;     copy this software, to redistribute it, and to use it for any
;;;     purpose is granted, subject to the following restrictions and
;;;     understandings.
;;;
;;;     1. Any copy made of this software must include this copyright
;;;     notice in full.
;;;
;;;     2. Users of this software agree to make their best efforts (a)
;;;     to return to the MIT Scheme project any improvements or
;;;     extensions that they make, so that these may be included in
;;;     future releases; and (b) to inform MIT of noteworthy uses of
;;;     this software.
;;;
;;;     3. All materials developed as a consequence of the use of this
;;;     software shall duly acknowledge such use, in accordance with
;;;     the usual standards of acknowledging credit in academic
;;;     research.
;;;
;;;     4. MIT has made no warrantee or representation that the
;;;     operation of this software will be error-free, and MIT is
;;;     under no obligation to provide any services, by way of
;;;     maintenance, update, or otherwise.
;;;
;;;     5. In conjunction with products arising from the use of this
;;;     material, there shall be no use of the name of the
;;;     Massachusetts Institute of Technology nor of any adaptation
;;;     thereof in any advertising, promotional, or sales literature
;;;     without prior written consent from MIT in each case.

;;;; X Graphics Interface

;;; Written by James Anderson.

(declare (usual-integrations))

;;; interface declarations and argument documentation
;;; please excuse the length of the description; it keeps all the information
;;; in one place. the notation is:
;;;    ( <function-name> <arguments ...> ) -> <values>
;;; a number of functions return different values depending on the display.
;;; this is the case where functions may not be implemented for a given
;;; display, or if the display interface established ideosyncratic
;;; conventions.
;;;
;;; the values include:
;;;    NIL             for a unsuccessful invocation, as would be the case
;;;                    when graphics is not initialized.
;;;    T               for functions which otherwise return no value
;;;    display-id      a handle on an open display
;;;    window-id       a handle on a window. these handles
;;;                    are display specific.
;;;    color-pixel-id  a handle on a color which may be used with draw
;;;                    operations. these are display specific.
;;;    color-tile-id   a handle on a color-tile which may be used with fill
;;;                    operations. these are display specific.
;;;    graphics-function       a display specific identifier for a
;;;                    function to be used to compute the displayed
;;;                    pixel values.
;;;    *-location      float or fixed quantities which specify coordinates
;;;                    xy values are returned as cons pairs.
;;;    buttons         mouse button state. bit mask for left, middle, right
;;;    cursor-id       a handle on an X cursor
;;;
;;; in most cases, the arguments should be generated by function evaluation.
;;; this will assure that application code is not hardware specific.
;;; the following  arguments are used to functions which generate the
;;; display specific handles:
;;;    display-name    "tty", "penplot", "pga", "pc", "X", NIL
;;;                    specify the display to be chosen, should options
;;;                    be available. NIL specifies the default display.
;;;    pattern-index   0 - 9 : selects from among the default patterns.
;;;    bit-pattern     is used to generate an X line pattern.
;;;    (red green blue)        are color levels, normalized to 65535
;;;    color-name      is the string representation of the name for a
;;;                    given color.
;;;    color-*-id      as generated by a get-* call, 0 to specify background,
;;;                    -1 to indicate foreground.
;;;    graphics-function       as generated by get-graphics-function, or
;;;                    NIL to indicate the display-function current for the
;;;                    current window.
;;;    cursor-bits	list of 16 16-bit values used to construct the cursor
;;;    cursor-name     cursor name. "cross", "null", and "arrow"
;;;                    are defined internally. other are located in
;;;                    /usr/include/X/cursors.

; standard graphics functions bindings
(define init-graphics)
(define reset-graphics)
(define clear-graphics)
(define draw-point)
(define clear-point)
(define is-point-on?)
(define position-pen)
(define draw-line-to)
(define graphics-draw)
(define graphics-draw-relative)
(define graphics-rectangle)
(define graphics-rectangle-relative)
(define graphics-poly)
(define graphics-poly-relative)
(define graphics-arc)
(define graphics-circle)
(define graphics-pixel)
(define graphics-label)
(define PCS-Machine-Type #!FALSE)

(define graphics-package
  (let
      ((graphics-initialize (make-primitive-procedure 'GRAPHICS-INITIALIZE))
       ;;(graphics-initialize display-name) -> [display-id | NIL]
       ;;
       (graphics-done (make-primitive-procedure 'GRAPHICS-DONE))
       ;;(graphics-done) -> [T | NIL]
       ;;
       (graphics-set-display! (make-primitive-procedure 'GRAPHICS-SET-DISPLAY!))
       ;;(graphics-set-display! display-id) -> [T | NIL]
       ;;
       (prim-graphics-open-window (make-primitive-procedure
				   'GRAPHICS-OPEN-WINDOW))
       ;;(prim-graphics-open-window x y w h) -> [window-id | NIL]
       ;;
       (graphics-set-window! (make-primitive-procedure 'GRAPHICS-SET-WINDOW!))
       ;;(graphics-set-window! window-id) -> [T | NIL]
       ;;
       (graphics-close-window (make-primitive-procedure 'GRAPHICS-CLOSE-WINDOW))
       ;;(graphics-close-window window-id) -> [T | NIL]
       ;;
       (prim-graphics-configure-window (make-primitive-procedure
					'GRAPHICS-CONFIGURE-WINDOW))
       ;;(prim-graphics-configure-window window-id x y w h) -> [T | NIL ]
       ;;
       (graphics-map-window (make-primitive-procedure 'GRAPHICS-MAP-WINDOW))
       ;;(graphics-map-window window-id) -> [ T | NIL ]
       ;;
       (graphics-unmap-window (make-primitive-procedure 'GRAPHICS-UNMAP-WINDOW))
       ;;(graphics-unmap-window window-id) -> [ T | NIL ]
       ;;
       (graphics-text (make-primitive-procedure 'GRAPHICS-TEXT))
       ;;(graphics-text) -> [T | NIL]
       ;;
       (graphics-graphics (make-primitive-procedure 'GRAPHICS-GRAPHICS))
       ;;(graphics-graphics) -> [T | NIL]
       ;;
       (graphics-set-flush! (make-primitive-procedure 'GRAPHICS-SET-FLUSH!))
       ;;(graphics-set-flush! [T | NIL]) -> [T | NIL]
       ;;
       (graphics-flush (make-primitive-procedure 'GRAPHICS-FLUSH))
       ;;(graphics-flush) -> [T | NIL]
       ;;
       ;;
       
       (graphics-clear (make-primitive-procedure 'GRAPHICS-CLEAR))
       ;;(graphics-clear) -> [T | NIL]
       ;;
       (graphics-pixfill (make-primitive-procedure 'GRAPHICS-PIXFILL))
       (graphics-flood (make-primitive-procedure 'GRAPHICS-PIXFILL))
       ;;(graphics-pixfil color-pixel-id) -> [T | NIL]
       ;;
       (graphics-tilefill (make-primitive-procedure 'GRAPHICS-TILEFILL))
       ;;(graphics-tilefill color-tile-id) -> [T | NIL]
       ;;
       (graphics-set-function! (make-primitive-procedure
				'GRAPHICS-SET-FUNCTION!))
       ;;(graphics-set-function graphics-function) -> [T | NIL]
       ;;
       (graphics-set-fill-function! (make-primitive-procedure
				     'GRAPHICS-SET-FILL-FUNCTION!))
       ;;(graphics-set-fill-function! graphics-function) -> [T | NIL]
       ;;
       (graphics-set-draw-function! (make-primitive-procedure
				     'GRAPHICS-SET-DRAW-FUNCTION!))
       ;;(graphics-set-draw-function! graphics-function) -> [T | NIL]
       ;;
       (graphics-set-poly-operation! (make-primitive-procedure
				      'GRAPHICS-SET-POLY-OPERATION!))
       ;;(graphics-set-poly-operation! polygon-operation) -> [T | NIL]
       ;;
       (graphics-set-draw-pattern! (make-primitive-procedure
				    'GRAPHICS-SET-DRAW-PATTERN!))
       ;;(graphics-set-draw-pattern! [pattern-index | bit-pattern]) -> [T | NIL]
       ;;
       (graphics-set-color! (make-primitive-procedure 'GRAPHICS-SET-COLOR!))
       ;;(graphics-set-color! [color-pixel-id | 0 ]) -> [T | NIL]
       ;;
       (graphics-set-draw-color! (make-primitive-procedure 
				  'GRAPHICS-SET-DRAW-COLOR!))
       ;;(graphics-set-draw-color! [color-pixel-id | 0 ]) -> [T | NIL]
       ;;
       (graphics-set-fill-color! (make-primitive-procedure 
				  'GRAPHICS-SET-FILL-COLOR!))
       ;;(graphics-set-fill-color! [color-tile-id | 0 ]) -> [T | NIL]
       ;;
       (graphics-set-pen!  (make-primitive-procedure 'GRAPHICS-SET-PEN!))
       ;;(graphics-set-pen! [ color-pixel-id | 0]) -> [T | NIL]
       ;;
       (graphics-define-pen (make-primitive-procedure 'GRAPHICS-DEFINE-PEN))
       ;;(graphics-define-pen [(pattern-index color-pixel-id) |            X
       ;;                      (pattern-index hue intensity saturation)] ) Penplot
       ;;                          -> [T | NIL]
       ;;
       (graphics-set-cursor! (make-primitive-procedure 'GRAPHICS-SET-CURSOR!))
       ;;(graphics-set-cursor! cursor-id) -> [ T | NIL ]
       ;;
       
       (graphics-get-window (make-primitive-procedure 'GRAPHICS-GET-WINDOW))
       ;;(graphics-get-draw-pattern) -> [ window-id | nil ]
       ;;
       (graphics-get-draw-pattern (make-primitive-procedure
				   'GRAPHICS-GET-DRAW-PATTERN))
       ;;(graphics-get-draw-pattern [ pattern-name | 0]) -> [pattern-index | NIL]
       ;;
       (graphics-get-draw-color (make-primitive-procedure
				 'GRAPHICS-GET-DRAW-COLOR))
       ;;(graphics-get-draw-color [(red green blue) | color-name |
       ;;          "foreground" | "background" ] ) -> [color-pixel-id | NIL]
       ;;
       (graphics-get-fill-color (make-primitive-procedure
				 'GRAPHICS-GET-FILL-COLOR))
       ;;(graphics-get-fill-color [(red green blue) | color-name |
       ;;          "foreground" | "background" ] ) -> [color-tile-id | NIL]
       ;;
       (prim-graphics-get-cursor (make-primitive-procedure
				  'GRAPHICS-GET-CURSOR))
       ;;(prim-graphics-get-cursor [ (bits-0 ... bits-n) | cursor-name ] 
       ;;                       graphics-function width height xoff yoff )
       ;;				-> cursor-id
       ;;
       (graphics-get-position (make-primitive-procedure 'GRAPHICS-GET-POSITION))
       ;;(graphics-get-position) -> [ (pen-x-location . pen-y-location) | NIL]
       ;;
       (graphics-get-window-size (make-primitive-procedure
				  'GRAPHICS-GET-WINDOW-SIZE))
       ;;(graphics-get-window-size) -> [(window-width . window-height) | NIL]
       ;;
       (graphics-get-window-position (make-primitive-procedure
				      'GRAPHICS-GET-WINDOW-POSITION))
       ;;(graphics-get-window-position) -> [(window-x . window-y) | NIL]
       ;;
       (graphics-get-mouse-position (make-primitive-procedure
				     'GRAPHICS-GET-MOUSE-POSITION))
       ;;(graphics-get-mouse-position [window-id | NIL]) ->
       ;;          [ (pen-x-location . pen-y-location) | NIL]
       ;;
       (graphics-get-mouse-buttons (make-primitive-procedure
				    'GRAPHICS-GET-MOUSE-BUTTONS))
       ;;(graphics-get-mouse-buttons [window-id | nil]) -> [ button-state | NIL ]
       ;;
       (graphics-get-mouse-window (make-primitive-procedure
				   'GRAPHICS-GET-MOUSE-WINDOW))
       ;;(graphics-get-mouse-window [window-id | NIL] ) -> [window-id | NIL]
       ;;
       (graphics-get-event (make-primitive-procedure
			    'GRAPHICS-GET-EVENT))
       ;;(graphics-get-event window-id type) -> [window-id | NIL]
       ;;    note that values passed in are modified
       ;;
       (graphics-get-event-position (make-primitive-procedure
				     'GRAPHICS-GET-EVENT-POSITION))
       ;;(graphics-get-event-position) -> [ (xloc . yloc) | NIL ]
       ;;
       (graphics-get-event-window (make-primitive-procedure
				   'GRAPHICS-GET-EVENT-WINDOW))
       ;;(graphics-get-event-window) -> [window-id | NIL]
       ;;
       (graphics-get-event-detail (make-primitive-procedure
				   'GRAPHICS-GET-EVENT-DETAIL))
       ;;(graphics-get-event-detail field) -> [field-value | NIL ]
       ;;
       (graphics-put-event (make-primitive-procedure
			    'GRAPHICS-PUT-EVENT))
       ;;(graphics-put-event window-id type args) -> [window-id | NIL]
       ;;
       (graphics-select-event (make-primitive-procedure
			       'GRAPHICS-SELECT-EVENT))
       ;;(graphics-select-event window-id type) -> [T | NIL]
       ;;
       (graphics-get-function (make-primitive-procedure
			       'GRAPHICS-GET-FUNCTION))
       ;;(graphics-get-function) -> [graphics-function | NIL]
       ;;
       ;;
       (graphics-move (make-primitive-procedure 'GRAPHICS-MOVE))
       ;;(graphics-move x-location y-location) -> [T | NIL]
       ;;
       (graphics-move-relative (make-primitive-procedure
				'GRAPHICS-MOVE-RELATIVE))
       ;;(graphics-move-relative x-displacement y-displacement) -> [T | NIL]
       ;;
       (prim-graphics-draw (make-primitive-procedure 'GRAPHICS-DRAW))
       ;;(graphics-draw to-x-location to-y-location
       ;;                  [graphics-function | NIL]) -> [T | NIL]
       ;;
       (prim-graphics-draw-relative (make-primitive-procedure
				     'GRAPHICS-DRAW-RELATIVE))
       ;;(graphics-draw to-x-displacement to-y-displacement
       ;;                  [graphics-function | NIL]) -> [T | NIL]
       ;;
       (prim-graphics-rectangle (make-primitive-procedure 'GRAPHICS-RECTANGLE))
       ;;(graphics-rectangle to-x-location to-y-location
       ;;                  [graphics-function | NIL]) -> [T | NIL]
       ;;
       (prim-graphics-rectangle-relative (make-primitive-procedure 
					  'GRAPHICS-RECTANGLE-RELATIVE))
       ;;(graphics-rectangle-relative to-x-displacement to-y-displacement
       ;;                  [graphics-function | NIL]) -> [T | NIL]
       ;;
       (prim-graphics-poly (make-primitive-procedure 'GRAPHICS-POLY))
       ;;(graphics-poly vertex-list [graphics-function | NIL])
       ;;
       (prim-graphics-poly-relative (make-primitive-procedure 'GRAPHICS-POLY-RELATIVE))
       ;;(graphics-poly-relative vertex-list [graphics-function | NIL])
       ;;
       (prim-graphics-arc (make-primitive-procedure 'GRAPHICS-ARC))
       ;;(graphics-arc radius start-angle end-angle 
       ;;                  [graphics-function | NIL]) -> [T | NIL]
       ;;
       (prim-graphics-circle (make-primitive-procedure 'GRAPHICS-CIRCLE))
       ;;(graphics-circle radius [graphics-function | NIL]) -> [T | NIL]
       ;;
       (prim-graphics-pixel (make-primitive-procedure 'GRAPHICS-PIXEL))
       ;;(graphics-pixel pixel-x-location pixel-y-location
       ;;                  [graphics-function | NIL] ) -> [T | NIL]
       ;;
       ;;
       
       (prim-graphics-label (make-primitive-procedure 'GRAPHICS-LABEL))
       ;;(graphics-label text-string) -> [T | NIL]
       ;;
       (graphics-set-letter (make-primitive-procedure 'GRAPHICS-SET-LETTER))
       ;;(graphics-set-letter (height aspect rotate slant))-> Nil          X
       ;;                                                  -> [T | NIL]    Penplot
       ;;
       (graphics-scl3 (make-primitive-procedure 'GRAPHICS-SCL3))
       ;;(graphics-scl3 ?) -> NIL                                          X
       ;;                  -> [T | NIL]                                    Penplot

       ;;
       (graphics-splot3 (make-primitive-procedure 'GRAPHICS-SPLOT3))
       ;;(graphics-splot3 ?) -> NIL                                        X
       ;;                  -> [T | NIL]                                    Penplot
       (toggle-alpha-raster (make-primitive-procedure 'GRAPHICS-TEXT))
       (toggle-graphics-raster (make-primitive-procedure 'GRAPHICS-GRAPHICS))
       (load-picture (make-primitive-procedure 'LOAD-PICTURE))
       ;;not yet implemented
       (store-picture (make-primitive-procedure 'STORE-PICTURE))
       ;;not yet implemented
       
       ;; manifest constants
       ;; fill and frame modes for polygon draw operations
       (NIL #!FALSE)
       (t #!TRUE)
       (*black* )
       (*white* )
       (*red-fill*)
       (*blue-fill*)
       (*green-fill*)
       (*cyan-fill*)
       (*magenta-fill*)
       (*yellow-fill*)
       (*white-fill*)
       (*black-fill*)
       (*gray-fill*)
       (*red-draw*)
       (*blue-draw*)
       (*green-draw*)
       (*cyan-draw*)
       (*magenta-draw*)
       (*yellow-draw*)
       (*white-draw*)
       (*black-draw*)
       (*gray-draw*)
       (*graphics-bkgd-color* )
       (*graphics-frgd-color* )
       (*graphics-color* )
       (*graphics-draw-pattern* 0)
       (*current-window* 0)
       (*current-display* 0)
       (*draw-pattern-masks* #(#b1111111111111111;; 0
			       #b0001000100010001;; 1
			       #b1111000011110000;; 2
			       #b1100101011001010;; 3
			       #b1110110011101100;; 4
			       #b1111010101111010;; 5
			       #b1110111011101110;; 6
			       #b1100110011001100;; 7
			       #b1101001101001100;; 8
			       #b1010101010101010;; 9
			       ))
       ;; polygon operations
       (*PolyFill* 1)
       (*PolyFrame* 2)
       (*PolyFrameFill* 3)
       (*TerminalType* 0)
       (*VT125* 1)
       (*VS100* 2)
       (*XTerm* 3)
       
       ;; X operation modes for graphics
       ;;/* Copyright    Massachusetts Institute of Technology    1985     */
<<<<<<< Xgraph.scm
       ;;/* $Header: Xgraph.scm,v 13.90 88/06/06 08:59:46 ajc Exp $ */
=======
       ;;/* $Header: Xgraph.scm,v 13.90 88/06/06 08:59:46 ajc Exp $ */
>>>>>>> 13.80.2.1
       ;;/* Definitions for the X window system to be used by applications */
       (*X_PROTOCOL* 10)		;                /* current protocol version */
       (*X_NoEvent* #x0000 )		;
       (*X_KeyPressed* #x0001 )		; /* keyboard key pressed */
       (*X_KeyReleased* #x0002 )	;        /* keyboard key released */
       (*X_ButtonPressed* #x0004 )	;      /* mouse button pressed */
       (*X_ButtonReleased* #x0008 )	;     /* mouse button released */
       (*X_EnterWindow* #x0010 )	;        /* mouse entering window */
       (*X_LeaveWindow* #x0020 )	;        /* mouse leaving window */
       (*X_MouseMoved* #x0040 )		; /* mouse moves within window */
       (*X_ExposeWindow* #x0080 )	; /* full window changed / exposed */
       (*X_ExposeRegion* #x0100 )	; /* region of window exposed */
       (*X_ExposeCopy* #x0200 )		; /* region exposed by X_CopyArea */
       (*X_RightDownMotion* #x0400 )	; /* mouse moves with right down */
       (*X_MiddleDownMotion* #x0800 )	; /* mouse moves with middle down */
       (*X_LeftDownMotion* #x1000 )	; /* mouse moves with left down */
       (*X_UnmapWindow* #x2000 )	;        /* window is unmapped */
       (*X_FocusChange* #x4000 )	;        /* keyboard focus changed */
       ;;
       ;;/* Event detail bits */
       ;;
       (*X_ControlMask* #x4000 )	;        /* Control key */
       (*X_MetaMask* #x2000 )		;   /* Meta (Symbol) key */
       (*X_ShiftMask* #x1000 )		;  /* Shift key */
       (*X_ShiftLockMask* #x0800 )	;      /* ShiftLock key */
       (*X_LeftMask* #x0400 )		;   /* Left button */
       (*X_MiddleMask* #x0200 )		; /* Middle button */
       (*X_RightMask* #x0100 )		;  /* Right button */
       (*X_ValueMask* #x00ff )		;  /* Key/button code */
       ;;
       ;;/* Button event detail codes */
       ;;
       (*X_RightButton* 0 )
       (*X_MiddleButton* 1 )
       (*X_LeftButton* 2 )
       ;;
       ;;/* Enter/Leave event detail codes */
       ;;
       (*X_IntoOrFromSubwindow* 1 )
       (*X_VirtualCrossing* 2 )
       ;;
       ;;/* for monochrome applications */
       ;;
       (*X_BlackPixel* 0 )		;        /* may not actually be black */
       (*X_WhitePixel* 1 )		;        /* may not actually be white */
       ;;
       ;;/* graphics functions */
       ;;
       (*X_GXclear* #x0 )		;       /* 0 */
       (*GXclear* #x0 )
       (*X_GXand* #x1 )			;         /* src AND dst */
       (*GXand* #x1 )
       (*X_GXandReverse* #x2 )		;  /* src AND NOT dst */
       (*GXandReverse* #x2 )
       (*X_GXcopy* #x3 )		;        /* src */
       (*GXcopy* #x3 )
       (*X_GXandInverted* #x4 )		; /* NOT src AND dst */
       (*GXandInverted* #x4 )
       (*X_GXnoop* #x5 )		;        /* dst */
       (*GXnoop* #x5 )
       (*X_GXxor* #x6 )			;         /* src XOR dst */
       (*GXxor* #x6 )
       (*X_GXor* #x7 )			;          /* src OR dst */
       (*GXor* #x7 )
       (*X_GXnor* #x8 )			;         /* NOT src AND NOT dst */
       (*GXnor* #x8 )
       (*X_GXequiv* #x9 )		;       /* NOT src XOR dst */
       (*GXequiv* #x9 )
       (*X_GXinvert* #xa )		;      /* NOT dst */
       (*GXinvert* #xa )
       (*X_GXorReverse* #xb )		;   /* src OR NOT dst */
       (*GXorReverse* #xb )
       (*X_GXcopyInverted* #xc )	;/* NOT src */
       (*GXcopyInverted* #xc )
       (*X_GXorInverted* #xd )		;  /* NOT src OR dst */
       (*GXorInverted* #xd )
       (*X_GXnand* #xe )		;        /* NOT src OR NOT dst */
       (*GXnand* #xe )
       (*X_GXset* #xf )			;         /* 1 */
       (*GXset* #xf )
       ;;
       ;;/* Used in X_TileMode */
       ;;
       (*X_TileModeAbsolute* 0 )
       (*X_TileModeRelative* 1 )
       ;;
       ;;/* Used in X_ClipMode */
       ;;
       (*X_ClipModeClipped* 0 )
       (*X_ClipModeDrawThru* 1 )
       )

;;;; higher level functions

    (define (graphics-get-color color)
      (graphics-get-draw-color color)
      )
    
    (define (set-color! color)
      (without-interrupts
       (lambda ()
         (graphics-set-color color)
         (set! *graphics-color* color)))
      )
    
    (define (set-draw-pattern! style)
      (without-interrupts
       (lambda ()
         (graphics-set-draw-pattern! style)
         (set! *graphics-draw-pattern* style)))
      )
    
    (define (with-line-style style thunk)
      (dynamic-wind (lambda ()
                      (graphics-set-draw-pattern! style))
                    thunk
                    (lambda ()
                      (graphics-set-draw-pattern! *graphics-draw-pattern*)))
      )

    (define (draw-line x y #!optional style)
      (if (unassigned? style)
          (graphics-draw (round x) (round y) #!FALSE)
          (graphics-draw (round x) (round y) style))
      )
    (define (draw-line-to x y #!optional style)
      (if (unassigned? style)
          (graphics-draw (round x) (round y) #!FALSE)
          (graphics-draw (round x) (round y) style))
      )
    
    (define (erase-line x y)
      (graphics-draw (round x) (round y) *GXinvert*)
      )
    (define (move-pen x y)
      (graphics-move (round x) (round y)))
    (define (position-pen x y)
      (graphics-move (round x) (round y)))
    
    (define (draw-pixel x y #!optional op)
      (if (unassigned? op)
          (graphics-pixel (round x) (round y) #!FALSE)
          (graphics-pixel (round x) (round y) op))
      )
    (define draw-point draw-pixel)
    (define (clear-point) (draw-pixel *GXclear*))
    

;;;; general graphics interface
    (set! graphics-draw
	  (lambda (x y #!optional mode)
	    (if (unassigned? mode)
		(set! mode '()))
	    (prim-graphics-draw x y mode))
	  )
    (set! graphics-draw-relative
	  (lambda (x y #!optional mode)
	    (if (unassigned? mode)
		(set! mode '()))
	    (prim-graphics-draw-relative x y mode))
	  )
    (set! graphics-rectangle
	  (lambda (x y #!optional mode)
	    (if (unassigned? mode)
		(set! mode '()))
	    (prim-graphics-rectangle x y mode))
	  )
    (set! graphics-rectangle-relative
	  (lambda (x y #!optional mode)
	    (if (unassigned? mode)
		(set! mode '()))
	    (prim-graphics-rectangle-relative x y mode))
	  )
    (set! graphics-poly
	  (lambda (poly #!optional mode)
	    (if (unassigned? mode)
		(set! mode '()))
	    (prim-graphics-poly poly mode))
	  )
    (set! graphics-poly-relative
	  (lambda (poly #!optional mode)
	    (if (unassigned? mode)
		(set! mode '()))
	    (prim-graphics-poly-relative poly mode))
	  )
    (set! graphics-arc
	  (lambda (radius angle angle #!optional mode)
	    (if (unassigned? mode)
		(set! mode '()))
	    (prim-graphics-arc radius angle angle mode))
	  )
    (set! graphics-circle
	  (lambda (radius #!optional mode)
	    (if (unassigned? mode)
		(set! mode '()))
	    (prim-graphics-circle radius mode))
	  )
    (set! graphics-pixel
	  (lambda (x y #!optional mode)
	    (if (unassigned? mode)
		(set! mode '()))
	    (prim-graphics-pixel x y mode))
	  )
    (set! graphics-label
	  (lambda (string)
	    (prim-graphics-label string))
	  )
    (define graphics-get-cursor
      (lambda (bits #!optional func w h x y)
	(if (unassigned? func)
	    (set! func *GXxor*))
	(if (unassigned? w)
	    (sequence (set! w 16)
		      (set! h 16)))
	(if (unassigned? x)
	    (sequence (set! x 0)
		      (set! y 0)))
	(prim-graphics-get-cursor bits func w h x y))
      )

;;;; Student Graphics Interface
    
    (set! draw-line-to
          (lambda (x y)
            (graphics-draw (round x) (round y))))
    
    (set! position-pen
          (lambda (x y)
            (graphics-move (round x) (round y))))
    
    (define ((graphics-point-procedure display-function) x y)
      (dynamic-wind
       (lambda () (graphics-pixel (round x) (round y)  display-function))))
    
    (define (graphics-get-mouse #!optional window-id)
      (if (unassigned? window-id)
	  (list (graphics-get-mouse-position #!FALSE)
		(graphics-get-mouse-buttons #!FALSE)
		(graphics-get-mouse-window #!FALSE))
	  (list (graphics-get-mouse-position window-id)
		(graphics-get-mouse-buttons window-id)
		(graphics-get-mouse-window window-id))))

    (set! draw-point (graphics-point-procedure *X_GXcopy*))

    (set! clear-point (graphics-point-procedure *X_GXclear*))

    (set! is-point-on? (graphics-point-procedure *X_GXnoop*))

    (set! clear-graphics
          (lambda ()
            (graphics-clear)
            (graphics-move 0 0))
          )
    (set! reset-graphics
          (lambda ()
            (clear-graphics)
            (graphics-set-flush! #!FALSE)
            (graphics-set-draw-pattern! 0)
            (graphics-set-function! *X_GXcopy*)
            (graphics-set-poly-operation! *PolyFill*))
          )
    
    (define (initialize-colors)
      (set! *red-fill* (graphics-get-fill-color "red"))
      (set! *blue-fill* (graphics-get-fill-color "blue"))
      (set! *green-fill* (graphics-get-fill-color "green"))
      (set! *cyan-fill* (graphics-get-fill-color "cyan"))
      (set! *magenta-fill* (graphics-get-fill-color "magenta"))
      (set! *yellow-fill* (graphics-get-fill-color "yellow"))
      (set! *white-fill* (graphics-get-fill-color "white"))
      (set! *black-fill* (graphics-get-fill-color "black"))
      (set! *gray-fill* (graphics-get-fill-color "light gray"))
      (set! *red-draw* (graphics-get-draw-color "red"))
      (set! *blue-draw* (graphics-get-draw-color "blue"))
      (set! *green-draw* (graphics-get-draw-color "green"))
      (set! *cyan-draw* (graphics-get-draw-color "cyan"))
      (set! *magenta-draw* (graphics-get-draw-color "magenta"))
      (set! *yellow-draw* (graphics-get-draw-color "yellow"))
      (set! *white-draw* (graphics-get-draw-color "white"))
      (set! *black-draw* (graphics-get-draw-color "black"))
      (set! *gray-draw* (graphics-get-draw-color "light gray"))
      (set! *white* *white-draw*)
      (set! *black* *black-draw*)
      (set! *graphics-bkgd-color* *black-fill*)
      (set! *graphics-frgd-color* *white-fill*)      
      (set! *graphics-color* *white-draw*)
      )

    (define (graphics-open-window #!optional x y w h)
      (if (unassigned? x)
	  (sequence (set! x '())
		    (set! y '())))
      (if (unassigned? w)
	  (sequence (set! w '())
		    (set! h '())))
      (prim-graphics-open-window x y w h))

    (define (graphics-configure-window #!optional x y w h)
      (if (unassigned? x)
	  (sequence (set! x '())
		    (set! y '())))
      (if (unassigned? w)
	  (sequence (set! w '())
		    (set! h '())))
      (prim-graphics-configure-window x y w h))

    
    (set! init-graphics
          (lambda (#!optional display)
	    (if (unassigned? display)
		(set! display ""))
            (set! *current-display* (graphics-initialize display))
	    (cond (*current-display*
		  (sequence
		    (initialize-colors)
		    (set! *current-window* (graphics-open-window)))
		  *current-display*)))
          )
    (the-environment)))


(define (my-init-graphics)
  (eval '(begin (set! *current-display* (graphics-initialize ""))
		(initialize-colors)
		(set! *current-window* (prim-graphics-open-window 2 2 556 412)))
	graphics-package)
  *the-non-printing-object*)

(define draw-line-to (eval 'draw-line-to graphics-package))

(define position-pen (eval 'position-pen graphics-package))

(define (set-pen! state)
  (eval `(graphics-set-draw-function! (cond ((eq? ',state 'draw) *GXcopy*)
					    ((eq? ',state 'erase) *GXclear*)
					    ((eq? ',state 'xor) *GXxor*)))
	graphics-package)
