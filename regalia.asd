(cl:in-package #:asdf-user)

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
  :components ((:module "code"
                :serial t
                :components ((:file "packages")
                             (:file "function.array-dimensions")))))
