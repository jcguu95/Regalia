;;;; Type SIMPLE-ARRAY
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/t_smp_ar.htm

(cl:in-package #:regalia)

;; NOTE
;;
;; According to the SPEC:
;;
;; > [1] The type of an array that is not displaced to another array, has no
;; > fill pointer, and is not expressly adjustable is a subtype of type
;; > simple-array.
;;
;; > [2] The concept of a simple array exists to allow the implementation to
;; > use a specialized representation and to allow the user to declare that
;; > certain values will always be simple arrays.
;;
;; The spec does not enforce type SIMPLE-ARRAY to satisfy more than what the
;; type ARRAY does. Hence I think it is enough for Regalia to provide the type
;; SIMPLE-ARRAY as an alias of the type ARRAY. This definition also satisfies
;; [1] as well. It is the responsibility of the client to ensure [1] if the
;; client decides to further restricts the type SIMPLE-ARRAY.

(deftype simple-array (&optional element-type dimension-spec)
  `(array ,element-type ,dimension-spec))
