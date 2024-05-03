;;;; Type SIMPLE-ARRAY
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/t_smp_ar.htm

(cl:in-package #:regalia)

;; NOTE SPEC:
;;
;; > [1] The type of an array that is not displaced to another array, has no
;; > fill pointer, and is not expressly adjustable is a subtype of type
;; > simple-array.
;;
;; > [2] The concept of a simple array exists to allow the implementation to
;; > use a specialized representation and to allow the user to declare that
;; > certain values will always be simple arrays.
;;
;; NOTE Notice the term "subtype". We decide to implement the type
;; SIMPLE-ARRAY as an alias of the type ARRAY for now.

;; TODO How to enforce "Compound Type Specifier Arguments"?
;;
;;   dimension — a valid array dimension.
;;   element-type — a type specifier.
;;   rank — a non-negative fixnum.

(deftype simple-array (&optional element-type dimension-spec)
  `(array ,element-type ,dimension-spec))
