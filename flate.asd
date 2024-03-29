(cl:in-package :cl-user)

(asdf:defsystem "flate"
  :version "0.0.3"
  :license "zlib"
  :description "DEFLATE decompressor"
  :author "Branan Landau"
  :mailto "blandau@posteo.net"
  :depends-on ()
  :components ((:file "package")
	       (:file "deflate" :depends-on ("package"))
	       (:file "decompress" :depends-on ("package" "deflate"))
	       (:file "zlib" :depends-on ("package" "decompress"))
	       (:file "gzip" :depends-on ("package" "decompress")))
  :in-order-to ((asdf:test-op (asdf:test-op "flate/test"))))

(asdf:defsystem "flate/test"
  :license "Public domain"
  :depends-on ("flate")
  :pathname "test/"
  :components ((:static-file "genome.txt") 
	       (:static-file "genome.gz")
	       (:file "test" :depends-on ("genome.txt" "genome.gz")))
  :perform (asdf:test-op (o c)
			 (uiop:symbol-call "FLATE-TEST" "RUN-TESTS")))

;; Test data is an excerpt from this entry in the US NIH genome database:
;; https://www.ncbi.nlm.nih.gov/datasets/genome/GCA_009829735.1/
