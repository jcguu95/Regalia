;; TODO Under Construction

;;;; Function VECTOR-PUSH-EXTEND
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_vec_ps.htm

(cl:in-package #:regalia)

(defgeneric vector-push-extend (new-element vector &optional extension))

;; TODO Reuse code of VECTOR-PUSH and ADJUST-ARRAY.
(defmethod vector-push-extend (new-element (vector vector) &optional extension)
  ;; (when (< (fill-pointer vector)
  ;;          (array-dimension vector 0))
  ;;   (let ((index (fill-pointer vector)))
  ;;     (incf (fill-pointer vector))
  ;;     (setf (aref index vector) new-element)
  ;;     index))
  )

(defmethod vector-push-extend (new-element vector &optional extension)
  (error 'type-error :datum array :expected-type 'array))

