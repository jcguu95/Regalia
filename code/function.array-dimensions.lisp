;;;; Function ARRAY-DIMENSIONS
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_ar_d_1.htm

(cl:in-package #:regalia)

;; NOTE Provided in class.array.lisp
;; (defmethod array-dimensions ((array array))
;;   (array-dimensions array))

(defmethod array-dimensions (array)
  (error 'type-error :datum array :expected-type 'array))
