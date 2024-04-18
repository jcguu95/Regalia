;;;; Function VECTOR-POP
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_vec_po.htm

(cl:in-package #:regalia)

(defgeneric vector-pop (vector))

(defmethod vector-pop ((vector vector))
  (if (and (array-has-fill-pointer-p vector)
           (> (fill-pointer vector) 0))

      (progn
        (decf (fill-pointer vector))
        (aref vector (1+ (fill-pointer vector))))

      ;; NOTE From the specification:
      ;; 1. An error of type type-error is signaled if vector does not have a fill pointer.
      ;; 2. If the fill pointer is zero, vector-pop signals an error of type error.
      ;; TODO Make this condition more complete.
      (error 'type-error)))

(defmethod vector-pop (vector)
  (error 'type-error :datum vector :expected-type 'vector))
