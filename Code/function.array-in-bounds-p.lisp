;;;; Function ARRAY-IN-BOUNDS-P
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_ar_in_.htm

(cl:in-package #:regalia)

(defgeneric array-in-bounds-p (array &rest subscripts))

(defmethod array-in-bounds-p ((array array) &rest subscripts)
  ;; NOTE #'array-row-major-index has a subroutine very similar to this. This is
  ;; an optimization opportunity.
  (loop :for d := dimensions :then (cdr d)
        :for s := subscripts :then (cdr s)
        :until (or (null d) (null s))
        :do (let ((s0 (car s))
                  (d0 (car d)))
              ;; TODO Clearly describe the failure.
              (assert (integerp s0))
              (unless (< -1 s0 d0) (return nil)))
        :finally
           (unless (and (null d) (null s))
             ;; Check both lists have the same length.
             ;; TODO Write a better signals:
             ;; Standard > subscripts---a list of integers of length equal to the rank of the array.
             (error "Lengths not equal."))
           (return t)))

(defmethod array-in-bounds-p (array &rest subscripts)
  (error 'type-error :datum array :expected-type 'array))
