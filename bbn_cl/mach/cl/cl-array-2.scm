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
(proclaim '(insert-touches nil))

(defun bit-array-boole (array1 array2 op result-array)
  (if (eq result-array t) (setq result-array array1))
  (setq array1 (touch array1))
  (setq array2 (touch array2))
  (setq result-array (touch result-array))
  (unless (bit-array-same-dimensions-p array1 array2)
	  (error "~S and ~S do not have the same dimensions." array1 array2))
  (if result-array
      (unless (bit-array-same-dimensions-p array1 result-array)
	      (error "~S and ~S do not have the same dimensions."
		     array1 result-array))
      (setq result-array (make-array (array-dimensions array1)
				     :element-type '(mod 2))))
  (with-array-data ((data1 array1) (start1) (end1))
    (declare (ignore end1))
    (with-array-data ((data2 array2) (start2) (end2))
      (declare (ignore end2))
      (with-array-data ((data3 result-array) (start3) (end3))
	(declare (ignore end3))
	(let ((length (array-total-size array1)))
	  (declare (fixnum length))
	  (do ((index 0 (1+ index))
	       (index1 start1 (1+ index1))
	       (index2 start2 (1+ index2))
	       (index3 start3 (1+ index3)))
	      ((= index length) result-array)
	    (declare (fixnum index index1 index2 index3))
	    (setf (sbit data3 index3)
		  (boole op (sbit data1 index1) (sbit data2 index2)))))))))

(defun bit-and (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical AND on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-array-boole bit-array1 bit-array2 boole-and result-bit-array))

(defun bit-ior (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical IOR on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-array-boole bit-array1 bit-array2 boole-ior result-bit-array))

(defun bit-xor (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical XOR on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-array-boole bit-array1 bit-array2 boole-xor result-bit-array))

(defun bit-eqv (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical EQV  on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-array-boole bit-array1 bit-array2 boole-eqv result-bit-array))

(defun bit-nand (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical NAND  on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-array-boole bit-array1 bit-array2 boole-nand result-bit-array))

(defun bit-nor (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical NOR  on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-array-boole bit-array1 bit-array2 boole-nor result-bit-array))

(defun bit-andc1 (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical ANDC1 on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-array-boole bit-array1 bit-array2 boole-andc1 result-bit-array))

(defun bit-andc2 (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical ANDC2 on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-array-boole bit-array1 bit-array2 boole-andc2 result-bit-array))

(defun bit-orc1 (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical ORC1 on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-array-boole bit-array1 bit-array2 boole-orc1 result-bit-array))

(defun bit-orc2 (bit-array1 bit-array2 &optional result-bit-array)
  "Performs a bit-wise logical ORC2 on the elements of Bit-Array1 and Bit-Array2
  putting the results in the Result-Bit-Array."
  (bit-array-boole bit-array1 bit-array2 boole-orc2 result-bit-array))

(defun bit-not (bit-array &optional result-bit-array)
  "Performs a bit-wise logical NOT in the elements of the Bit-Array putting
  the results into the Result-Bit-Array."
  (bit-array-boole bit-array bit-array boole-nor result-bit-array))
