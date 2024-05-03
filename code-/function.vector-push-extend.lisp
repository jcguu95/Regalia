;;;; Function VECTOR-PUSH-EXTEND
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_vec_ps.htm

(cl:in-package #:regalia)

;; NOTE SPEC: extension — a positive integer. The default is implementation-dependent.
(defconstant +vector-push-extend/extension+ 1)

(defgeneric vector-push-extend (new-element vector &optional extension))

(defmethod vector-push-extend (new-element (vector vector) &optional (extension +vector-push-extend/extension+))
  ;; NOTE SPEC: extension — a positive integer. The default is implementation-dependent.
  (check-type extension (integer 1))
  ;; NOTE SPEC: An error of type error is signaled if vector does not have a
  ;; fill pointer.
  (unless (fill-pointer vector)
    ;; TODO Write a better condition. It has to be a type error.
    (error))
  ;; vector-push returning NIL means that the vector is full and needs to be
  ;; adjusted. In this case, vector-push should not produce any other
  ;; side-effects.
  (let ((index (vector-push new-element vector)))
    (or index
        (progn
          ;; NOTE SPEC: An error of type error is signaled by vector-push-extend if it
          ;; tries to extend vector and vector is not actually adjustable.
          (unless (fill-pointer vector)
            ;; TODO Write a better condition. It has to be a type error.
            (error))
          (unless (adjustable-array-p vector)
            ;; TODO Write a better condition. It has to be a type error.
            (error))
          (let ((index (1+ (fill-pointer vector))))
            (adjust-array vector (list (+ extension (array-dimension vector 0)))
                          :initial-element +default-initial-element+)
            (setf (aref index vector) new-element)
            index)))))

(defmethod vector-push-extend (new-element vector &optional extension)
  (error 'type-error :datum array :expected-type 'array))

