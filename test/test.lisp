(cl:in-package :cl-user)

(defpackage :flate-test
  (:use :cl)
  (:export run-tests))

(in-package :flate-test)

(defparameter *raw-path* (asdf:system-relative-pathname "flate/test" "test/genome.txt"))
(defparameter *compressed-path* (asdf:system-relative-pathname "flate/test" "test/genome.gz"))

(defun decompression-test ()
  "Decompressed and uncompressed data are equal"
  (let* ((raw-file (open *raw-path* :element-type '(unsigned-byte 8)))
	 (compressed-file (open *compressed-path* :element-type '(unsigned-byte 8)))
	 (buffer (make-array (file-length raw-file) :element-type '(unsigned-byte 8)
						    :fill-pointer 0
						    :adjustable t)))
    (flate:decompress-gzip :input (lambda () (read-byte compressed-file))
			   :output (lambda (byte) (vector-push-extend byte buffer)))
    (close compressed-file)
    (prog1 (and (= (length buffer) (file-length raw-file))
		(every (lambda (byte) (= byte (read-byte raw-file))) buffer))
      (close raw-file))))

(defun run-tests ()
  (dolist (test '(decompression-test))
    (format t "~A: " (documentation test 'function))
    (format t "~A" (handler-case (funcall test)
		     (error (e) (symbol-name (class-name (class-of e))))
		     (:no-error (pass) (if pass "ok" "fail"))))
    (terpri)))
