;;;; Function BIT-AND, BIT-ANDC1, BIT-ANDC2, BIT-EQV, BIT-IOR, BIT-NAND, BIT-NOR, BIT-NOT, BIT-ORC1, BIT-ORC2, BIT-XOR
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_bt_and.htm

(defun check-dimension-match (bit-array1 bit-array2)
  ;; TODO Better describe the condition.
  (assert
   (or (not bit-array2)
       (equal (array-dimension bit-array1)
              (array-dimension bit-array2)))))

(defmacro define-bit-binary-operation (suffix)
  `(progn
     (defgeneric ,(intern (format nil "BIT-~a" suffix))
         (bit-array1
          bit-array2
          &optional opt-arg))
     (defmethod ,(intern (format nil "BIT-~a" suffix))
         ((bit-array1 bit-array)
          (bit-array2 bit-array)
          &optional opt-arg)
       (check-dimension-match bit-array1 bit-array-2)
       (check-dimension-match bit-array1 opt-arg)
       (let* ((resulting-bit-array (or opt-arg (make-array (array-dimension bit-array1))))
              (size (array-total-size bit-array1))
              (a1 (make-array size :displaced-to bit-array1))
              (a2 (make-array size :displaced-to bit-array2))
              (ar (make-array size :displaced-to resulting-bit-array)))
         (loop :for index :from 0 :to (1- size)
               :do (setf (aref ar index)
                         (,(intern (format nil "LOG~a" suffix))
                          (aref a1 index)
                          (aref a2 index))))
         resulting-bit-array))))

(define-bit-binary-operation and)
(define-bit-binary-operation andc1)
(define-bit-binary-operation andc2)
(define-bit-binary-operation eqv)
(define-bit-binary-operation ior)
(define-bit-binary-operation nand)
(define-bit-binary-operation nor)
(define-bit-binary-operation not)
(define-bit-binary-operation orc1)
(define-bit-binary-operation orc2)
(define-bit-binary-operation xor)

(defgeneric bit-not (bit-array &optional opt-arg))
(defmethod bit-not ((bit-array bit-array) &optional opt-arg)
  (check-dimension-match bit-array opt-arg)
  (let* ((resulting-bit-array (or opt-arg (make-array (array-dimension bit-array))))
         (size (array-total-size bit-array))
         (a (make-array size :displaced-to bit-array))
         (ar (make-array size :displaced-to resulting-bit-array)))
    (loop :for index :from 0 :to (1- size)
          :do (setf (aref ar index) (lognot (aref a index))))
    resulting-bit-array))
