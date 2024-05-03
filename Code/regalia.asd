(cl:in-package #:asdf-user)

(defsystem "regalia"
  :serial t
  :components
  ((:file "packages")))

(defsystem "regalia"
  :description "Portable ARRAY implementation for Common Lisp."
  :license "BSD"
  :author ("Robert Strandh"
           "Jin-Cheng Guu")
  :maintainer "Jin-Cheng Guu"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/jcguu95/Regalia"
  :bug-tracker "https://github.com/jcguu95/Regalia/issues"
  :depends-on ()
  :components ((:module "Code"
                :serial t
                :components ((:file "packages")
                             (:file "TODO")
                             (:file "conditions")
                             (:file "interface")))))

(defsystem "regalia-extrinsic"
  :description "Extrinsic interface to Regalia."
  :license "BSD"
  :author ("Robert Strandh"
           "Jin-Cheng Guu")
  :maintainer "Jin-Cheng Guu"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/jcguu95/Regalia"
  :bug-tracker "https://github.com/jcguu95/Regalia/issues"
  :depends-on ("regalia")
  ;; :in-order-to ((asdf:test-op (asdf:test-op #:regalia-extrinsic/test)))
  :components ((:module code
                :pathname "code/extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))

(asdf:defsystem "regalia-extrinsic/test"
  :description "Extrinsic testing interface to Regalia."
  :license "BSD"
  :author ("Robert Strandh"
           "Jin-Cheng Guu")
  :maintainer "Jin-Cheng Guu"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/jcguu95/Regalia"
  :bug-tracker "https://github.com/jcguu95/Regalia/issues"
  :depends-on ("regalia-extrinsic")
  :perform (asdf:test-op (op c)
             (symbol-call :regalia-extrinsic/test :test))
  :components ((:module code
                :pathname "code/extrinsic/test/"
                :serial t
                :components ((:file "packages")
                             (:file "ansi-test")
                             (:static-file "expected-failures.sexp")))))
