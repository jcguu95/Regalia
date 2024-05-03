;;;; Function VECTOR-PUSH
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_vec_ps.htm

(cl:in-package #:regalia)

(defgeneric vector-push (new-element vector))

(defmethod vector-push (new-element (vector vector))
  (when (< (fill-pointer vector)
           (array-dimension vector 0))
    (let ((index (fill-pointer vector)))
      (incf (fill-pointer vector))
      (setf (aref index vector) new-element)
      index)))

(defmethod vector-push (new-element vector)
  (error 'type-error :datum array :expected-type 'array))
