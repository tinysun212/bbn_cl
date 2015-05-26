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
;;;(declare (usual-integrations))

;;; Floating point assembler stuff.
;;;
;;; (%ge lap-syntax-package)
;;; (%gst assembler-syntax-table)

(define-symbol-transformer
  flt-src-fmt
  (L . 0)				; long word integer
  (S . 1)				; single precision real
  (X . 2)				; extended precision real
  (P . 3)				; packed decimal real
  (W . 4)				; word integer
  (D . 5)				; double precision real
  (B . 6)				; byte integer
  )

(define-symbol-transformer
  flt-dst-fmt
  (L . 0)				; long word integer
  (S . 1)				; single precision real
  (X . 2)				; extended precision real
  (W . 4)				; word integer
  (D . 5)				; double precision real
  (B . 6)				; byte integer
  )

(define-symbol-transformer flt-reg
  (FP0 . 0) (FP1 . 1) (FP2 . 2) (FP3 . 3)
  (FP4 . 4) (FP5 . 5) (FP6 . 6) (FP7 . 7))

(define-symbol-transformer flt-ctl-reg
  (FPCR . 4) (FPSR . 2) (FPIAR 1))

(define-symbol-transformer flt-cc
  (EQ . 1) (NE . 14) (GT . 2) (NGT . 13)
  (GE . 3) (NGE . 12) (LT . 4) (NLT . 11)
  (LE . 5) (NLE . 10) (GL . 6) (NGL . 9)
  (MI . 4) (PL . 3)
  (GLE . 7) (NGLE . 8) (F . 0) (T . 15))

(define-instruction FMOVE

  (((? type flt-src-fmt) (? src ea-d) (? dst flt-reg))
   (WORD (4 #b1111)
	 (3 #b001)   ; floating coprocessor
	 (3 #b000)
	 (6 src SOURCE-EA 'L))
   (EXTENSION-WORD (3 #b010)
		   (3 type)
		   (3 dst)
		   (7 #b0000000)))
  
  (((? src flt-reg) (? dst flt-reg))
   (WORD (4 #b1111)
	 (3 #b001) ; floating coprocessor
	 (3 #b000)
	 (6 #b000000))
   (EXTENSION-WORD (3 #b000)
		   (3 src)
		   (3 dst)
		   (7 #b0000000)))

  (((? type flt-dst-fmt) (? src flt-reg) (? dst ea-d&a))
   (WORD (4 #b1111)
	 (3 #b001) ; floating coprocessor
	 (3 #b000)
	 (6 dst DESTINATION-EA 'L))
   (EXTENSION-WORD (3 #b011)
		   (3 type)
		   (3 src)
		   (7 #b0000000)))

  (((P (? k-factor)) (? src flt-reg) (? dst ea-d&a))
   (WORD (4 #b1111)
	 (3 #b001) ; floating coprocessor
	 (3 #b000)
	 (6 dst DESTINATION-EA 'L))
   (EXTENSION-WORD (3 #b011)
		   (3 #b011)
		   (3 src)
		   (7 k-factor)))

  (((PD (? k-reg)) (? src flt-reg) (? dst ea-d&a))
   (WORD (4 #b1111)
	 (3 #b001) ; floating coprocessor
	 (3 #b000)
	 (6 dst DESTINATION-EA 'L))
   (EXTENSION-WORD (3 #b011)
		   (3 #b111)
		   (3 src)
		   (3 k-reg)
		   (4 #b0000)))

  ((L (? src ea-d) (? dst flt-ctl-reg))
   (WORD (4 #b1111)
	 (3 #b001) ; floating coprocessor#b000)
	 (3 #b000)
	 (6 src SOURCE-EA 'L))
   (EXTENSION-WORD (3 #b100)
		   (3 dst)
		   (10 #b0000000000)))

  ((L (? src flt-ctl-reg) (? dst ea-d))
   (WORD (4 #b1111)
	 (3 #b001) ; floating coprocessor
	 (3 #b000)
	 (6 dst DESTINATION-EA 'L))
   (EXTENSION-WORD (3 #b101)
		   (3 src)
		   (10 #b0000000000)))

  )

(define-global-macro (define-unary-flop name bits)
  `(define-instruction ,name

     (((? type flt-src-fmt) (? src ea-d) (? dst flt-reg))
      (WORD (4 #b1111)
	    (3 #b001) ; floating coprocessor
	    (3 #b000)
	    (6 src SOURCE-EA 'L))
      (EXTENSION-WORD (3 #b010)
		      (3 type)
		      (3 dst)
		      (7 ,bits)))

     (((? src flt-reg) (? dst flt-reg))
      (WORD (4 #b1111)
	    (3 #b001) ; floating coprocessor
	    (3 #b000)
	    (6 #b000000))
      (EXTENSION-WORD (3 #b000)
		      (3 src)
		      (3 dst)
		      (7 ,bits)))

     (((? reg flt-reg))
      (WORD (4 #b1111)
	    (3 #b001) ; floating coprocessor
	    (3 #b000)
	    (6 #b000000))
      (EXTENSION-WORD (3 #b000)
		      (3 reg)
		      (3 reg)
		      (7 ,bits)))))


(define-unary-flop FABS	   #b0011000)
(define-unary-flop FACOS   #b0011100)
(define-unary-flop FASIN   #b0001100)
(define-unary-flop FATAN   #b0001010)
(define-unary-flop FATANH  #b0001101)
(define-unary-flop FCOS    #b0011101)
(define-unary-flop FCOSH   #b0011001)
(define-unary-flop FETOX   #b0010000)
(define-unary-flop FETOXM1 #b0001000)
(define-unary-flop FGETEXP #b0011110)
(define-unary-flop FGETMAN #b0011111)
(define-unary-flop FINT    #b0000001)
(define-unary-flop FINTRZ  #b0000011)
(define-unary-flop FLOG10  #b0010101)
(define-unary-flop FLOG2   #b0010110)
(define-unary-flop FLOGN   #b0010100)
(define-unary-flop FLOGNP1 #b0000110)
(define-unary-flop FNEG    #b0011010)
(define-unary-flop FSIN    #b0001110)
(define-unary-flop FSINH   #b0000010)
(define-unary-flop FSQRT   #b0000100)
(define-unary-flop FTAN    #b0001111)
(define-unary-flop FTANH   #b0001001)
(define-unary-flop FTENTOX #b0010010)
(define-unary-flop FTWOTOX #b0010001)

(define-global-macro (define-binary-flop name bits)
  `(define-instruction ,name

     (((? type flt-src-fmt) (? src ea-d) (? dst flt-reg))
      (WORD (4 #b1111)
	    (3 #b001) ; floating coprocessor
	    (3 #b000)
	    (6 src SOURCE-EA 'L))
      (EXTENSION-WORD (3 #b010)
		      (3 type)
		      (3 dst)
		      (7 ,bits)))

     (((? src flt-reg) (? dst flt-reg))
      (WORD (4 #b1111)
	    (3 #b001) ; floating coprocessor
	    (3 #b000)
	    (6 #b000000))
      (EXTENSION-WORD (3 #b000)
		      (3 src)
		      (3 dst)
		      (7 ,bits)))
))

(define-binary-flop FADD    #b0100010)
(define-binary-flop FCMP    #b0111000)
(define-binary-flop FDIV    #b0100000)
(define-binary-flop FMOD    #b0100001)
(define-binary-flop FMUL    #b0100011)
(define-binary-flop FREM    #b0100101)
(define-binary-flop FSCALE  #b0100110)
(define-binary-flop FSGLDIV #b0100100)
(define-binary-flop FSGLMUL #b0100111)
(define-binary-flop FSUB    #b0101000)

(define-instruction FTST

  (((? type flt-src-fmt) (? ea ea-d))
   (WORD (4 #b1111)
	 (3 #b001) ; floating coprocessor
	 (3 #b000)
	 (6 ea SOURCE-EA 'L))
   (EXTENSION-WORD (3 #b010)
		   (3 type)
		   (3 #b000)
		   (7 #b0111010)))

  (((? src flt-reg))
   (WORD (4 #b1111)
	 (3 #b001) ; floating coprocessor
	 (3 #b000)
	 (6 #b000000))
   (EXTENSION-WORD (3 #b000)
		   (3 src)
		   (3 #b000)
		   (7 #b0111010)))
  )

(define-instruction FB

  (((? cc flt-cc) (@PCR (? target)))
   (GROWING-WORD (disp `(- ,target (+ *PC* 2)))
		 ((-32768 32767)
		  (WORD (4 #b1111)
			(3 #b001) ; floating coprocessor
			(3 #b010)
			(6 cc)
			(16 disp SIGNED)))
		 ((() ())
		  (WORD (4 #b1111)
			(3 #b001) ; floating coprocessor
			(3 #b011)
			(6 cc)
			(32 disp SIGNED))))))

;;; Not handled:
;;;	FDBcc
;;;	FMOVECR
;;;	FMOVEM
;;;	FRESTORE
;;;	FSAVE
;;;	FScc
;;;	FSINCOS
;;;	FTRAPcc
