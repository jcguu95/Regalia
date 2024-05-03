(cl:in-package #:asdf-user)

(asdf:defsystem "regalia-extrinsic"
  :description "Extrinsic interface to Regalia."
  :license "BSD"
  :author ("Robert Strandh"
           "Jin-Cheng Guu")
  :maintainer "Jin-Cheng Guu"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/jcguu95/Regalia"
  :bug-tracker "https://github.com/jcguu95/Regalia/issues"
  :depends-on ("regalia")
  :in-order-to ((asdf:test-op (asdf:test-op #:regalia-extrinsic/test))) ; TODO What is this?
  :components ((:module "code"
                :pathname "code/extrinsic/"
                :serial t
                :components ((:file "packages")))))

(asdf:defsystem "regalia-extrinsic/test"
  :description "Extrinsic testing interface to Regalia."
  :license "BSD"
  :author ("Robert Strandh"
           "Jin-Cheng Guu")
  :maintainer "Jin-Cheng Guu"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/jcguu95/Regalia"
  :bug-tracker "https://github.com/jcguu95/Regalia/issues"
  :depends-on ("regalia-extrinsic"
               "ansi-test-harness")
  :perform (asdf:test-op (op c)
             (symbol-call :regalia-extrinsic/test :test))
  :components ((:module "code"
                :pathname "code/extrinsic/test/"
                :serial t
                :components ((:file "packages")
                             (:file "ansi-test")
                             (:static-file "expected-failures.sexp")))))
