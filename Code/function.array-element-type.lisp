;;;; Function ARRAY-ELEMENT-TYPE
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_ar_ele.htm

(cl:in-package #:regalia)

(defgeneric array-element-type (array))

(defmethod array-element-type ((array array))
  (%array-element-type array))

(defmethod array-element-type (array)
  (error 'type-error :datum array :expected-type 'array))
