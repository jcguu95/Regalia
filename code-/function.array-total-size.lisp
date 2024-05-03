;;;; Function ARRAY-TOTAL-SIZE
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_ar_tot.htm

(cl:in-package #:regalia)

(defgeneric array-total-size (array))

;; NOTE SPEC: array total size n. the total number of elements in an array,
;; computed by taking the product of the dimensions of the array. (The size of
;; a zero-dimensional array is therefore one.)
(defmethod array-total-size ((array array))
  (apply #'* (array-dimensions array)))

(defmethod array-total-size (array)
  (error 'type-error :datum array :expected-type 'array))
