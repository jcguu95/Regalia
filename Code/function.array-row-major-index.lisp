;;;; Function ARRAY-ROW-MAJOR-INDEX
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_ar_row.htm

(cl:in-package #:regalia)

(defgeneric array-row-major-index (array &rest subscripts))

(defmethod array-row-major-index ((array array) &rest subscripts)
  (let ((dimensions (array-dimensions array))
        (dimensions-reverse nil)
        (subscripts-reverse nil))
    ;; NOTE The standard does not define the behavior of subscripts overflow
    ;; or mis-specification. TODO We need to create a condition class to
    ;; describe that.
    (loop :for d := dimensions :then (cdr d)
          :for s := subscripts :then (cdr s)
          :until (or (null d) (null s))
          :do (let ((s0 (car s))
                    (d0 (car d)))
                ;; Ensure no subscripts overflow.
                (assert (and (integerp s0) (< -1 s0 d0))) ; TODO Clearly describe the failure.
                (push s0 subscripts-reverse)
                (push d0 dimensions-reverse))
          :finally
             ;; Check both lists have the same length.
             (unless (and (null d) (null s))
               ;; TODO Write a better signals:
               (error "Lengths not equal.")))
    ;; Algorithm - Let the dimensions of ARRAY be (d_1 d_2 [..] d_n) and the
    ;; subscripts be (s_1 s_2 [..] s_n). Then the row major index is
    ;; s_1*(d_2*..*d_n) + s_2(d_3*d_4*..*d_n) + .. + s_{n-1}*(d_n) + sn.
    (let ((result 0)
          (product 1))
      (loop :for d :in dimensions-reverse
            :for s :in subscripts-reverse
            :do (incf result (* product s))
                (setf product (* product d)))
      result)))

;;; Tests
;; TODO Remove this from file.
(assert (= 9 (array-row-major-index
              (make-array '(2 3 4)
                          :element-type '(unsigned-byte 8)
                          :displaced-to a
                          :displaced-index-offset 4)
              0 2 1)))

(assert (= 21 (array-row-major-index
               (make-array '(2 3 4)
                           :element-type '(unsigned-byte 8)
                           :displaced-to a
                           :displaced-index-offset 4)
               1 2 1)))

;;;
(defmethod array-row-major-index (array &rest subscripts)
  (error 'type-error :datum array :expected-type 'array))
