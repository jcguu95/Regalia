;;;; Accessor SVREF
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_svref.htm

(cl:in-package #:regalia)

(defgeneric svref (simple-vector index))

(defmethod svref ((simple-vector vector) (index fixnum))
  (check-type simple-vector simple-vector)
  (aref simple-vector index))

(defmethod svref (simple-vector index)
  (error 'type-error :datum simple-vector :expected-type 'simple-vector))

(defun (setf svref) (new-element simple-vector index)
  (check-type simple-vector simple-vector)
  (setf (row-major-aref simple-vector
                        (array-row-major-index simple-vector index))
        new-element))
