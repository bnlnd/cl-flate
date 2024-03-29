(cl:in-package :flate)

(defparameter max-history (ash 1 15))

(defparameter code-length-order
  #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))

(defparameter litlen-base
  #(3 4 5 6 7 8 9 10 11 13
    15 17 19 23 27 31 35 43 51 59
    67 83 99 115 131 163 195 227 258))

(defparameter litlen-extra-bits
  #(0 0 0 0 0 0 0 0 1 1
    1 1 2 2 2 2 3 3 3 3
    4 4 4 4 5 5 5 5 0))

(defparameter distance-base
  #(1 2 3 4 5 7 9 13 17 25
    33 49 65 97 129 193 257 385 513 769
    1025 1537 2049 3073 4097 6145 8193 12289 16385 24577))

(defparameter distance-extra-bits
  #(0 0 0 0 1 1 2 2 3 3
    4 4 5 5 6 6 7 7 8 8
    9 9 10 10 11 11 12 12 13 13))

(defclass codebook ()
  ((table :type hash-table
	  :initarg :table
	  :reader code-table)
   (max-bits :type integer
	     :initarg :max-bits
	     :reader max-bits)))

(defun code-ref (codebook code)
  (gethash code (code-table codebook)))

(defun rev-byte (b len)
  (do ((i 0 (1+ i))
       (n 0 (dpb (ldb (byte 1 i) b) (byte 1 (- len 1 i)) n)))
      ((= len i) n)))

(defparameter reverse-table
  (do ((a (make-array 256 :element-type '(unsigned-byte 8)))
       (i 0 (1+ i)))
      ((= i 256) a)
    (setf (aref a i) (rev-byte i 8))))

(defun make-codebook (code-lengths)
  (let* ((max (reduce #'max code-lengths))
	 (next-code (make-array (1+ max)))
	 (bl-count (make-array max))
	 (table (make-hash-table)))
    (dotimes (i (1- max)) 
      (setf (aref bl-count (1+ i)) (count (1+ i) code-lengths)))
    (dotimes (i max)
      (setf (aref next-code (1+ i))
	    (ash (+ (aref next-code i) (aref bl-count i)) 1)))
    (dotimes (i (length code-lengths))
      (let ((length (aref code-lengths i)))
	(unless (zerop length)
	  (when (logbitp length (aref next-code length))
	    (error 'malformed))
	  (setf (gethash (dpb length (byte 5 17) (rev-byte (aref next-code length) length)) table) i)
	  (incf (aref next-code length)))))
    (make-instance 'codebook :max-bits max :table table)))

(defparameter fixed-litlen-codebook
  (make-codebook (concatenate 'vector
			      (make-array 144 :initial-element 8)
			      (make-array 112 :initial-element 9)
			      (make-array 24 :initial-element 7)
			      (make-array 8 :initial-element 8))))

(defparameter fixed-distance-codebook
  (make-codebook (make-array 32 :initial-element 5)))

(defclass history ()
  ((buffer :type (array (unsigned-byte 8))
	   :initarg :buffer
	   :reader history-buffer)
   (pointer :type integer
	    :initform 0
	    :accessor history-pointer)))

(defun reset-history (history)
  (setf (fill-pointer (history-buffer history)) 0
	(history-pointer history) 0))

(defun history-full-p (history)
  (= (fill-pointer (history-buffer history))
     (array-total-size (history-buffer history))))

(defun history+ (n a)
  (mod (+ a n) max-history))

(defun history-put (history byte)
  (if (history-full-p history)
      (setf (aref (history-buffer history) (history-pointer history)) byte)
      (vector-push byte (history-buffer history)))
  (setf (history-pointer history)
	(history+ (history-pointer history) 1)))
