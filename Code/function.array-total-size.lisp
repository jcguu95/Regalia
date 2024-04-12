;;;; Function ARRAY-TOTAL-SIZE
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_ar_tot.htm

(cl:in-package #:regalia)

(defgeneric array-total-size (array))

(defmethod array-total-size ((array array))
  (apply #'* (array-dimensions array)))

(defmethod array-total-size (array)
  (error 'type-error :datum array :expected-type 'array))
