;;;; Function ADJUSTABLE-ARRAY-P
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_adju_1.htm

(cl:in-package #:regalia)

(defmethod adjustable-array-p (array)
  (error 'type-error :datum array :expected-type 'array))
