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
