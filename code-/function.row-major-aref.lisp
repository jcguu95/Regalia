;;;; Accessor ROW-MAJOR-AREF
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_row_ma.htm

(cl:in-package #:regalia)

(defgeneric row-major-aref (array index))

;; Only for extrinsic version. For intrinsic use, the host has to provide its
;; own #'MAKE-ARRAY-CONTENTS and #'ROW-MAJOR-AREF.
(defmethod row-major-aref ((array array) (index integer))
  (cl:svref (array-contents array) index))

(defmethod row-major-aref (array index)
  (error 'type-error :datum array :expected-type 'array))

(defun (setf row-major-aref) (new-element array index)
  (setf (cl:svref (array-contents array) index) new-element))
