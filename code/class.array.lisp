;;;; System Class ARRAY
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/t_array.htm

(cl:in-package #:regalia)

(defgeneric array-contents (array))
(defgeneric array-dimensions (array))
(defgeneric array-element-type (array))
(defgeneric adjustable-array-p (array))
(defgeneric array-displaced-to (array))
(defgeneric array-displaced-index-offset (array))

(defclass array ()
  ((%contents :initarg :contents :accessor array-contents) ; Only for the extrinsic version. See #'make-array-contents.
   (%dimensions :initarg :dimensions :reader array-dimensions)
   ;; NOTE https://novaspec.org/cl/f_adjustable-array-p
   ;; NOTE https://novaspec.org/cl/26_1_Glossary#actually_adjustable
   (%actually-adjustable :initarg :adjustable :reader adjustable-array-p)
   ;; NOTE https://novaspec.org/cl/26_1_Glossary#expressly_adjustable
   (%expressly-adjustable :initarg :expressly-adjustable)
   ;; TODO Enforcement (?) and Documentation: The slot of %element-type has to
   ;; be a type-specifier https://www.lispworks.com/documentation/HyperSpec/Body/26_glo_t.htm#type_specifier
   (%element-type
    :initform t
    :initarg :element-type
    :accessor array-element-type)
   (%displaced-to
    :initform nil
    :initarg :displaced-to
    :reader array-displaced-to)
   (%displaced-index-offset
    :initform 0
    :initarg :displaced-index-offset
    :reader array-displaced-index-offset)))

;; NOTE - ARRAY as a type specifier:
;;
;; The client is expected to provide the type ARRAY, which is like a "special
;; type operator" because ARRAY should act as an atomic type specifier
;; (defined via defclass) and a compound type specifier.
