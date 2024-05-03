(cl:in-package #:common-lisp-user)

(defpackage #:regalia-extrinsic
  (:use #:common-lisp)
  (:shadow #:array-dimensions)
  (:export #:array-dimensions))
