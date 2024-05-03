;;;; Type SIMPLE-VECTOR
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/t_smp_ve.htm

(cl:in-package #:regalia)

(deftype simple-vector (&optional size)
  ;; NOTE SPEC: size - a non-negative fixnum, or the symbol *. The default is
  ;; the symbol *.
  ;;
  ;; TODO How to enforce this?
  `(and
    ;; NOTE SPEC: The type simple-vector [..] is a subtype of type (vector t).
    (vector t)
    ;; NOTE SPEC: The type of a vector that is not displaced to another array,
    ;; has no fill pointer, is not expressly adjustable and is able to hold
    ;; elements of any type is a subtype of type simple-vector.
    (simple-array t size)))
