;;;; Type SIMPLE-BIT-VECTOR
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/t_smp_bt.htm

(cl:in-package #:regalia)

(deftype simple-bit-vector (&optional size)
  ;; NOTE SPEC: size - a non-negative fixnum, or the symbol *. The default is
  ;; the symbol *.
  ;;
  ;; TODO How to enforce this?
  `(and
    ;; NOTE SPEC: The type simple-vector [..] is a subtype of type (vector t).
    (bit-vector t)
    ;; NOTE SPEC: The type of a bit vector that is not displaced to another
    ;; array, has no fill pointer, and is not expressly adjustable is a
    ;; subtype of type simple-bit-vector.
    (simple-array bit size)))
