(cl:in-package #:common-lisp-user)

(defpackage #:regalia
  (:use #:common-lisp)
  ;; #+sicl (:local-nicknames (#:closer-mop #:sicl-clos))
  (:shadow)
  (:export #:array-dimensions))
