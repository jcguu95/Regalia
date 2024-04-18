;;;; Function ARRAY-DISPLACEMENT
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_ar_dis.htm

(cl:in-package #:regalia)

(defgeneric array-displacement ((array array))
  (let ((displaced-to (array-displaced-to array))
        (displaced-index-offset (array-displaced-index-offset array)))
    (if displaced-to
        (values displaced-to displaced-index-offset)
        (values nil 0))))

(defmethod array-displacement (array)
  (error 'type-error :datum array :expected-type 'array))
