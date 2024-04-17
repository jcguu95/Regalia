;;;; System Class ARRAY
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/t_array.htm

(cl:in-package #:regalia)

(defgeneric array-contents (array))
(defgeneric array-dimensions (array))
(defgeneric array-element-type (array))
(defgeneric adjustable-array-p (array))

(defclass array ()
  ((%contents :initarg :contents :accessor array-contents) ; Only for the extrinsic version. See #'make-array-contents.
   (%dimensions :initarg :dimensions :reader array-dimensions)
   (%adjustable :initarg :adjustable :reader adjustable-array-p)
   ;; TODO Enforcement (?) and Documentation: The slot of %element-type has to
   ;; be a type-specifier https://www.lispworks.com/documentation/HyperSpec/Body/26_glo_t.htm#type_specifier
   (%element-type
    :initform t
    :initarg :element-type
    :accessor array-element-type)))

;; NOTE - ARRAY as a type specifier:
;;
;; The client is expected to provide the type ARRAY, which is like a "special
;; type operator" because ARRAY should act as an atomic type specifier
;; (defined via defclass) and a compound type specifier.
