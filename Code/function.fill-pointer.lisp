;;;; Accessor FILL-POINTER
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_fill_p.htm

;; NOTE The method for ((vector vector)) is provided in class.vector.lisp.

(defmethod fill-pointer (vector)
  (error 'type-error :datum array :expected-type 'array))
