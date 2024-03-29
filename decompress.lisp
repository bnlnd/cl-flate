(cl:in-package :flate)

(define-condition malformed (error)
  ((format :type string
	   :initarg :format
	   :initform "DEFLATE"
	   :reader malformed-format))
  (:report (lambda (c s)
	     (format s "~A data is malformed" (malformed-format c)))))

(defclass decompressor (history) ())

(defclass decompression-state ()
  ((history :type decompressor
	    :initarg :buffer
	    :reader history)
   (bits :type integer
	 :initform 0
	 :accessor bit-buffer)
   (bit-pointer :type integer
		:initform 8
		:accessor bit-pointer)
   (reader :type function
	   :initarg :reader
	   :reader byte-reader)
   (writer :type function
	   :initarg :writer
	   :reader byte-writer)))

(defun make-decompressor ()
  (make-instance 'decompressor
		 :buffer (make-array max-history
				     :element-type '(unsigned-byte 8)
				     :fill-pointer 0)))

(defun history-copy (decompressor distance length)
  (do ((index (history+ (history-pointer (history decompressor)) (- distance)) (history+ 1 index))
       (i 0 (1+ i)))
      ((= i length))
    (byte-put decompressor (aref (history-buffer (history decompressor)) index))))

(defun byte-get (decompressor)
  (funcall (byte-reader decompressor)))

(defun byte-put (decompressor byte)
  (funcall (byte-writer decompressor) byte)
  (history-put (history decompressor) byte))

(defun clear-bits (decompressor)
  (setf (bit-pointer decompressor) 8))

(defun load-bits (decompressor)
  (setf (bit-pointer decompressor) 0
	(bit-buffer decompressor) (byte-get decompressor)))

(defun 1bit (decompressor)
  (when (= 8 (bit-pointer decompressor))
    (load-bits decompressor))
  (prog1 (ldb (byte 1 (bit-pointer decompressor)) (bit-buffer decompressor))
    (incf (bit-pointer decompressor))))

(defun nbits (decompressor n)
  (do ((i 0 (1+ i))
       (int 0 (dpb (1bit decompressor) (byte 1 i) int)))
      ((= i n) int)))

(defun read-code (decompressor codebook)
  (do ((i 0 (1+ i))
       (bits (ash 1 17)))
      ((= i (max-bits codebook)) (error 'malformed))
    (setf bits (dpb (1+ i) (byte 5 17) bits))
    (setf bits (dpb (1bit decompressor) (byte 1 i) bits))
    (multiple-value-bind (value status) (code-ref codebook bits)
      (when status
	(return value)))))

(defun read-length (decompressor code)
  (unless (>= 285 code 257)
    (error 'malformed))
  (+ (aref litlen-base (- code 257))
     (nbits decompressor (aref litlen-extra-bits (- code 257)))))

(defun read-distance (decompressor code)
  (when (or (= 30 code) (= 31 code))
    (error 'malformed))
  (+ (aref distance-base code)
     (nbits decompressor (aref distance-extra-bits code))))

(defun decode (decompressor litlen-table distance-table)
  (do ()
      (nil)
    (let ((litlen-code (read-code decompressor litlen-table)))
      (cond ((= 256 litlen-code) (return))
	    ((> 256 litlen-code) (byte-put decompressor litlen-code))
	    (t (let ((length (read-length decompressor litlen-code)))
		 (unless (>= 258 length 3)
		   (error 'malformed))
		 (let* ((distance-code (read-code decompressor distance-table))
			(distance (read-distance decompressor distance-code)))
		   (unless (>= max-history distance 1)
		     (error 'malformed))
		   (history-copy decompressor distance length))))))))

(defun read-dynamic-table (decompressor)
  (let ((litlen-count (+ 257 (nbits decompressor 5)))
	(distance-count (+ 1 (nbits decompressor 5)))
	(code-length-count (+ 4 (nbits decompressor 4)))
	(code-lengths (make-array 19)))
    (dotimes (i code-length-count)
      (setf (aref code-lengths (aref code-length-order i)) (nbits decompressor 3)))
    (let ((length-table (make-codebook code-lengths))
	  (length-vector (make-array (+ litlen-count distance-count) :fill-pointer 0)))
      (do ()
	  ((= (length length-vector) (array-total-size length-vector)))
	(multiple-value-bind (value length)
	    (let ((code (read-code decompressor length-table)))
	      (unless (>= 18 code 0)
		(error 'malformed))
	      (case code
		(16 (values (aref length-vector (1- (length length-vector)))
			    (+ 3 (nbits decompressor 2))))
		(17 (values 0 (+ 3 (nbits decompressor 3))))
		(18 (values 0 (+ 11 (nbits decompressor 7))))
		(t (values code 1))))
	  (dotimes (i length)
	    (vector-push value length-vector))))
      (values (make-codebook (subseq length-vector 0 litlen-count))
	      (make-codebook (subseq length-vector litlen-count))))))

(defun decode-fixed-data (decompressor)
  (decode decompressor fixed-litlen-codebook fixed-distance-codebook))

(defun decode-dynamic-data (decompressor)
  (multiple-value-bind (litlen distance) (read-dynamic-table decompressor)
    (decode decompressor litlen distance)))

(defun decode-uncompressed-data (decompressor)
  (clear-bits decompressor)
  (let ((len (logior (byte-get decompressor) (ash (byte-get decompressor) 8)))
	(nlen (logior (byte-get decompressor) (ash (byte-get decompressor) 8))))
    (unless (= -1 (logeqv len (lognot nlen)))
      (error 'malformed))
    (dotimes (i len)
      (byte-put decompressor (byte-get decompressor)))))

(defun decode-block (decompressor)
  (let ((bfinal (1bit decompressor))
	(btype (nbits decompressor 2)))
    (funcall (case btype
	       (#b00 #'decode-uncompressed-data)
	       (#b01 #'decode-fixed-data)
	       (#b10 #'decode-dynamic-data)
	       (t (error 'malformed)))
	     decompressor)
    (= 1 bfinal)))

(defun decompress (&key input output (decompressor (make-decompressor)))
  (reset-history decompressor)
  (do ((state (make-instance 'decompression-state :reader input
						  :writer output
						  :buffer decompressor)))
      ((decode-block state))))

