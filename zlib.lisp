(cl:in-package :flate)

(defmacro zmust (form)
  `(unless ,form
     (error 'malformed :format "zlib")))

(defun u32 (input)
  (logior (ash (funcall input) 24)
	  (ash (funcall input) 16)
	  (ash (funcall input) 8)
	  (funcall input)))

(defun read-zlib-header (input)
  (let ((cmf (funcall input))
	(flg (funcall input)))
    (zmust (zerop (mod (dpb cmf (byte 8 8) flg) 31)))
    (zmust (= 8 (ldb (byte 4 0) cmf)))
    (when (logbitp 5 flg)
      (u32 input))))

(defun read-zlib-footer (input adler32)
  (zmust (= (u32 input) adler32)))

(defun decompress-zlib (&key input output (decompressor (make-decompressor)))
  (read-zlib-header input)
  (let ((s1 1)
	(s2 0))
    (decompress :input input
		:output (lambda (byte)
			  (setf s1 (mod (+ s1 byte) 65521)
				s2 (mod (+ s2 s1) 65521))
			  (funcall output byte))
		:decompressor decompressor)
    (read-zlib-footer input (dpb s2 (byte 16 16) s1))))
