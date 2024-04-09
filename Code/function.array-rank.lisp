;;;; Function ARRAY-RANK
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_ar_ran.htm
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/26_glo_r.htm#rank

(cl:in-package #:regalia)

(defgeneric array-rank (array))

(defmethod array-rank ((array array))
  (length (array-dimensions array)))

(defmethod array-rank (array)
  (error 'type-error :datum array :expected-type 'array))
