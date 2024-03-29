(cl:in-package :cl-user)

(defpackage :flate
  (:use :cl)
  (:export make-decompressor
	   decompress
	   decompress-gzip
	   decompress-zlib))
