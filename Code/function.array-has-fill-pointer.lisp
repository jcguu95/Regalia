;;;; Function ARRAY-HAS-FILL-POINTER
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_ar_has.htm

(cl:in-package #:regalia)

(defgeneric array-has-fill-pointer (array))

;; NOTE From https://www.lispworks.com/documentation/HyperSpec/Body/f_mk_ar.htm
;;
;; > If fill-pointer is non-nil, the array must be one-dimensional; that is,
;; > the array must be a vector. If fill-pointer is t, the length of the
;; > vector is used to initialize the fill pointer. If fill-pointer is an
;; > integer, it becomes the initial fill pointer for the vector.

(defmethod array-has-fill-pointer ((array array)) nil)

(defmethod array-has-fill-pointer ((array vector))
  (fill-pointer array))

(defmethod array-has-fill-pointer (array)
  (error 'type-error :datum array :expected-type 'array))
