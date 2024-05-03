;;;; Function VECTOR
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_vector.htm

(cl:in-package #:regalia)

;; FIXME Find out if this will create a vector, or just a mere array?
(defun vector (&rest objects)
  (make-array (list (length objects))
              :element-type t
              :initial-contents objects))
