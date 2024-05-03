(in-package #:regalia-extrinsic/test)

(defun test ()
  (let ((system (asdf:find-system :regalia-extrinsic/test)))
    (ansi-test-harness:ansi-test
     :directory (merge-pathnames
                 (make-pathname :directory '(:relative "dependencies" "ansi-test"))
                 (asdf:component-pathname system))
     :expected-failures (asdf:component-pathname
                         (asdf:find-component system '("code" "expected-failures.sexp")))
     :extrinsic-symbols '(regalia-extrinsic:array-dimensions)
     :tests '("ARRAY-DIMENSIONS")
     :exit nil)))
