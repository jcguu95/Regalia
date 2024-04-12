;;;; System Class VECTOR
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/t_vector.htm

(defclass vector (array sequence)
  ((%fill-pointer
    :initform nil
    :initarg :fill-pointer
    :accessor vector-fill-pointer)))

(defun vectorp (object)
  (typep object 'vector))
