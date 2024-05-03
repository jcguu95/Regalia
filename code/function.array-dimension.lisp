;;;; Function ARRAY-DIMENSION
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_ar_dim.htm

(cl:in-package #:regalia)

;; NOTE Depends on the function ARRAY-RANK.

;; NOTE The spec does requires AXIS-NUMBER to be an integer greater than or
;; equal to zero and less than the rank of the array. However, it does not
;; define the behavior otherwise. We signal a type error.

;; TODO Define a subtype of TYPE-ERROR with its own :REPORT function that is
;; more precise for explaning the situation.

(defun array-dimension (array axis-number)
  (let ((rank (array-rank array)))
    (unless (and (integerp axis-number)
                 (< -1 axis-number rank))
      (error 'type-error
             :datum axis-number
             :expected-type `(integer 0 ,rank))))
  (nth n (array-dimensions array)))

