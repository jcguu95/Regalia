;;;; Function MAKE-ARRAY
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_mk_ar.htm

(cl:in-package #:regalia)

;; NOTE Some code taken and edited from
;; https://github.com/robert-strandh/SICL/blob/master/Code/Array/make-array-defun.lisp

(defun %compute-array-spec (element-type rank element-count)
  "Returns three values - class-name, additional-space, and default-element."
  ;; TODO Find out if we can reuse code of UPGRADED-ARRAY-ELEMENT-TYPE.
  (cond ((eq element-type 't)
         (values (if (= rank 1) 'simple-vector 'array-t)
                 element-count
                 nil))
        ((subtypep element-type 'character)
         (values (if (= rank 1) 'string 'array-character)
                 (ceiling element-count 2)
                 #\Space))
        ((subtypep element-type 'bit)
         (values (if (= rank 1) 'bit-vector 'array-bit)
                 (ceiling element-count 64)
                 0))
        ((subtypep element-type '(unsigned-byte 8))
         (values (if (= rank 1) 'vector-unsigned-byte-8 'array-unsigned-byte-8)
                 (ceiling element-count 8)
                 0))
        ((subtypep element-type '(signed-byte 32))
         (values (if (= rank 1) 'vector-signed-byte-32 'array-signed-byte-32)
                 (ceiling element-count 2)
                 0))
        ((subtypep element-type '(unsigned-byte 32))
         (values (if (= rank 1) 'vector-unsigned-byte-32 'array-unsigned-byte-32)
                 (ceiling element-count 2)
                 0))
        ((subtypep element-type '(signed-byte 64))
         (values (if (= rank 1) 'vector-signed-byte-64 'array-signed-byte-64)
                 element-count
                 0))
        ((subtypep element-type '(unsigned-byte 64))
         (values (if (= rank 1) 'vector-unsigned-byte-64 'array-unsigned-byte-64)
                 element-count
                 0))
        ((subtypep element-type 'single-float)
         (values (if (= rank 1) 'vector-single-float 'array-single-float)
                 (ceiling element-count 2)
                 0f0))
        ((subtypep element-type 'double-float)
         (values (if (= rank 1) 'vector-double-float 'array-double-float)
                 element-count
                 0d0))
        ((subtypep element-type '(complex single-float))
         (values (if (= rank 1) 'vector-complex-single-float 'array-complex-single-float)
                 element-count
                 #c(0f0 0f0)))
        ((subtypep element-type '(complex double-float))
         (values (if (= rank 1) 'vector-complex-double-float 'array-complex-double-float)
                 (* element-count 2)
                 #c(0d0 0d0)))
        (t
         (values (if (= rank 1) 'simple-vector 'array-t)
                 element-count
                 nil))))

(defun make-array (dimensions
                   &key
                     (element-type 't)
                     (initial-element nil initial-element-p)
                     (initial-contents nil initial-contents-p)
                     (adjustable)
                     (fill-pointer)
                     (displaced-to nil displaced-to-p)
                     (displaced-index-offset nil displaced-index-offset-p))
  ;; TODO handle ADJUSTABLE, DISPLACED-TO, DISPLACED-INDEX-OFFSET
  (declare (ignore adjustable displaced-to displaced-index-offset))
  ;; FIXME Do some more checks on the dimensions.
  (let* ((canonicalized-dimensions (if (listp dimensions)
                                       dimensions
                                       (list dimensions)))
         (rank (length canonicalized-dimensions))
         (element-count (if canonicalized-dimensions
                            (apply #'* canonicalized-dimensions)
                            0)))
    (multiple-value-bind (class-name additional-space default-element)
        (%compute-array-spec element-type rank element-count)
      (let ((new-array
              ;; FIXME Rank may be zero.
              (if (= rank 1)
                  (make-instance class-name
                                 :dimensions canonicalized-dimensions
                                 :additional-space additional-space
                                 :fill-pointer (if (eq fill-pointer 't)
                                                   element-count
                                                   fill-pointer))
                  (make-instance class-name
                                 :dimensions canonicalized-dimensions
                                 :additional-space additional-space))))
        (if initial-contents-p
            (let ((index 0))
              (labels ((init (dimensions contents)
                         (cond ((null dimensions)
                                (setf (row-major-aref new-array 0) contents))
                               ((null (rest dimensions))
                                (if (listp contents)
                                    (loop for element in contents
                                          repeat (first dimensions)
                                          do (setf (row-major-aref new-array index) element)
                                             (incf index))
                                    (loop for element across contents
                                          repeat (first dimensions)
                                          do (setf (row-major-aref new-array index) element)
                                             (incf index))))
                               (t
                                (if (listp contents)
                                    (loop for element in contents
                                          repeat (first dimensions)
                                          do (init (rest dimensions) element))
                                    (loop for element across contents
                                          repeat (first dimensions)
                                          do (init (rest dimensions) element)))))))
                (init canonicalized-dimensions initial-contents)))
            (let ((element (if initial-element-p initial-element default-element)))
              (loop for index from 0 below element-count
                    do (setf (row-major-aref new-array index) element))))
        new-array))))
