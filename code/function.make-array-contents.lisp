;;;; Function MAKE-ARRAY-CONTENTS
;;;;
;;;; This is not a part of the standard. It serves as a primary element.

(cl:in-package #:regalia)

;; NOTE This is for the extrinsic version. To use intrinsically, the host has
;; to provide #'MAKE-ARRAY-CONTENTS.
(defun make-array-contents (xs)
  "Make array contents from the list XS."
  (cl:make-array xs))
