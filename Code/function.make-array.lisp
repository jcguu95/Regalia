;;;; Function MAKE-ARRAY
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_mk_ar.htm

;; NOTE MAKE-ARRAY is a function whose decision model of behavior on its key
;; arguments is not simple. When cleaning up the repo, make sure to read the
;; spec line by line, and add missing tests to ANSI-TEST if necessary.

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
                     (adjustable nil)
                     (fill-pointer nil)
                     (displaced-to nil displaced-to-p)
                     (displaced-index-offset 0 displaced-index-offset-p))
  ;; NOTE
  ;;
  ;; SPEC: dimensions — a designator for a list of valid array dimensions.
  ;;
  ;; SPEC: valid array dimension n. a fixnum suitable for use as an array
  ;; dimension. Such a fixnum must be greater than or equal to zero, and less
  ;; than the value of array-dimension-limit. When multiple array dimensions
  ;; are to be used together to specify a multi-dimensional array, there is
  ;; also an implied constraint that the product of all of the dimensions be
  ;; less than the value of array-total-size-limit.
  ;;
  ;; TODO Check the above.

  ;; NOTE SPEC: If make-array is called with adjustable, fill-pointer, and
  ;; displaced-to each nil, then the result is a simple array. If make-array
  ;; is called with one or more of adjustable, fill-pointer, or displaced-to
  ;; being true, whether the resulting array is a simple array is
  ;; implementation-dependent. TODO [TYPE]

  ;; Normalize ADJUSTABLE.
  (when adjustable (setf adjustable t))

;;; Some preliminary checks.

  ;; NOTE SPEC: If initial-element is supplied, it must be of the type given
  ;; by element-type.
  (check-type initial-element element-type)

  ;; NOTE SPEC: displaced-to — an array or nil.
  (check-type displaced-to (or array null))

  ;; NOTE SPEC: initial-element cannot be supplied if either the
  ;; :initial-contents option is supplied or displaced-to is non-nil.
  (when (and initial-element-p (or initial-contents-p displaced-to))
    ;; TODO Write a better condition signal. [CONDITION]
    (error))
  ;; NOTE SPEC: Initial-contents cannot be supplied if either initial-element
  ;; is supplied or displaced-to is non-nil.
  (when (and initial-contents-p (or initial-element-p displaced-to))
    ;; TODO Write a better condition signal. [CONDITION]
    (error))
  ;; NOTE SPEC: This option [displaced-to] must not be supplied if either
  ;; initial-element or initial-contents is supplied. (DONE above.)

  ;; NOTE SPEC: displaced-index-offset - a valid array row-major index for
  ;; displaced-to. The default is 0. This option must not be supplied unless a
  ;; non-nil displaced-to is supplied.
  ;;
  ;; NOTE SPEC: The total number of elements in an array, called the total
  ;; size of the array, is calculated as the product of all the dimensions. It
  ;; is required that the total size of A be no smaller than the sum of the
  ;; total size of B plus the offset n supplied by the displaced-index-offset.
  (if displaced-index-offset-p
      (if (and displaced-to displaced-to-p)
          ;; TODO Write a better condition signal. [CONDITION]
          (assert
           (>= (array-total-size displaced-to)
               (+ displaced-index-offset (apply #'* dimensions))))
          ;; TODO Write a better condition signal. [CONDITION]
          (error)))

  ;; NOTE SPEC: If initial-element is not supplied, the
  ;; consequences of later reading an uninitialized element of
  ;; new-array are undefined unless either initial-contents is
  ;; supplied or displaced-to is non-nil.
  ;;
  ;; NOTE SPEC: If initial-contents is not supplied, the
  ;; consequences of later reading an uninitialized element of
  ;; new-array are undefined unless either initial-element is
  ;; supplied or displaced-to is non-nil.
  (when (and displaced-to
             (not initial-element-p)
             (not initial-contents-p))
    ;; TODO Write a better condition signal. [CONDITION]
    (warn "You've reached an undefined area."))

;;;

  ;; FIXME Do some more checks on the dimensions.
  (let* ((canonicalized-dimensions (if (listp dimensions)
                                       dimensions
                                       (list dimensions)))
         (rank (length canonicalized-dimensions))
         (element-count (if canonicalized-dimensions
                            (apply #'* canonicalized-dimensions)
                            0)))

    ;; NOTE SPEC: fill-pointer — a valid fill pointer for the array to be
    ;; created, or t or nil. WHERE: valid fill pointer n. (of an array) a
    ;; fixnum suitable for use as a fill pointer for the array. Such a fixnum
    ;; must be greater than or equal to zero, and less than or equal to the
    ;; array total size of the array.
    (check-type fill-pointer (or boolean fixnum))
    ;; NOTE SPEC: If fill-pointer is non-nil, the array must be one-dimensional;
    ;; that is, the array must be a vector.
    (when fill-pointer
      (unless (= 1 (length canonicalized-dimensions))
        ;; TODO Signal better conditions. [CONDITION]
        (error)))

    (multiple-value-bind (class-name additional-space default-element)
        (%compute-array-spec element-type rank element-count)
      (let ()
        (if initial-element-p
            (progn
              ;; NOTE SPEC: If initial-element is supplied, it is used to
              ;; initialize each element of new-array. [DONE]

              ;; NOTE SPEC: If initial-element is supplied, it must be of the
              ;; type given by element-type. [DONE]

              ;; NOTE SPEC: initial-element cannot be supplied if either the
              ;; :initial-contents option is supplied or displaced-to is
              ;; non-nil. [DONE]

              )
            (progn
              ;; NOTE SPEC: If initial-element is not supplied, the
              ;; consequences of later reading an uninitialized element of
              ;; new-array are undefined unless either initial-contents is
              ;; supplied or displaced-to is non-nil. Question: What does this
              ;; mean? Answer: After discussing with bike, it seems that the
              ;; standard means instead "uninitialized slot/cell of
              ;; new-array". [DONE]
              ))

        (if initial-contents-p
            (progn
              ;; NOTE SPEC: initial-contents is composed of a nested structure
              ;; of sequences. [DONE]

              ;; NOTE SPEC: The numbers of levels in the structure must equal
              ;; the rank of array. [DONE]

              ;; NOTE SPEC: Each leaf of the nested structure must be of the
              ;; type given by element-type. [DONE]

              ;; NOTE SPEC: If array is zero-dimensional, then
              ;; initial-contents specifies the single element. [DONE]

              ;; NOTE SPEC: Otherwise, initial-contents must be a sequence
              ;; whose length is equal to the first dimension; each element
              ;; must be a nested structure for an array whose dimensions are
              ;; the remaining dimensions, and so on. [DONE]

              ;; NOTE SPEC: Initial-contents cannot be supplied if either
              ;; initial-element is supplied or displaced-to is non-nil.
              ;; [DONE]

              ;; NOTE SPEC: If initial-contents is not supplied, the
              ;; consequences of later reading an uninitialized element of
              ;; new-array are undefined unless either initial-element is
              ;; supplied or displaced-to is non-nil. -- Question: What does
              ;; this mean? Answer: After discussing with bike, it seems that
              ;; the standard means instead "uninitialized slot/cell of
              ;; new-array". [DONE]
              ))

        (cond ((not (null fill-pointer))
               ;; NOTE SPEC: If fill-pointer is non-nil, the array must be
               ;; one-dimensional; that is, the array must be a vector. DONE
               )
              ((eq t fill-pointer)
               ;; NOTE SPEC: If fill-pointer is t, the length of the vector is
               ;; used to initialize the fill pointer. DONE
               )
              ((integerp fill-pointer)
               ;; NOTE SPEC: If fill-pointer is an integer, it becomes the initial
               ;; fill pointer for the vector. DONE
               ))

        (if displaced-to
            (progn
              ;; NOTE SPEC: If displaced-to is non-nil, make-array will create a displaced
              ;; array and displaced-to is the target of that displaced array. DONE

              ;; NOTE SPEC: [If displaced-to is non-nil], the consequences are
              ;; undefined if the actual array element type of displaced-to is not
              ;; type equivalent to the actual array element type of the array being
              ;; created. TODO [TYPE]
              )
            (progn
              ;; NOTE SPEC: If displaced-to is nil, the array is not a displaced
              ;; array. DONE
              ))

        ;; NOTE DISPLACED-INDEX-OFFSET
        ;;
        ;; The displaced-index-offset is made to be the index offset of the
        ;; array. DONE
        ;;
        ;; When an array A is given as the :displaced-to argument to
        ;; make-array when creating array B, then array B is said to be
        ;; displaced to array A. DONE
        ;;
        ;; The total number of elements in an array, called the total size of
        ;; the array, is calculated as the product of all the dimensions. It
        ;; is required that the total size of A be no smaller than the sum of
        ;; the total size of B plus the offset n supplied by the
        ;; displaced-index-offset. DONE
        ;;
        ;; The effect of displacing is that array B does not have any elements
        ;; of its own, but instead maps accesses to itself into accesses to
        ;; array A.
        ;;
        ;; The mapping treats both arrays as if they were one-dimensional by
        ;; taking the elements in row-major order, and then maps an access to
        ;; element k of array B to an access to element k+n of array A.

        (let ((index 0)
              (new-array
                ;; FIXME Rank may be zero, in each case the resulting array is
                ;; 0-dimensional, and can hold one object.
                (if (= rank 1)
                    (make-instance class-name
                                   :dimensions canonicalized-dimensions
                                   :additional-space additional-space
                                   :fill-pointer (if (eq fill-pointer 't)
                                                     element-count
                                                     fill-pointer)
                                   :displaced-to displaced-to
                                   :displaced-index-offset displaced-index-offset
                                   ;; NOTE SPEC: If adjustable is non-nil, the
                                   ;; array is expressly adjustable (and so
                                   ;; actually adjustable); otherwise, the
                                   ;; array is not expressly adjustable (and
                                   ;; it is implementation-dependent whether
                                   ;; the array is actually adjustable).
                                   :adjustable adjustable
                                   :expressly-adjustable adjustable)
                    (make-instance class-name
                                   :dimensions canonicalized-dimensions
                                   :additional-space additional-space
                                   :displaced-to displaced-to
                                   :displaced-index-offset displaced-index-offset
                                   ;; NOTE SPEC: If adjustable is non-nil, the
                                   ;; array is expressly adjustable (and so
                                   ;; actually adjustable); otherwise, the
                                   ;; array is not expressly adjustable (and
                                   ;; it is implementation-dependent whether
                                   ;; the array is actually adjustable).
                                   :adjustable adjustable
                                   :expressly-adjustable adjustable))))
          (labels ((populate-with-element (dimensions element)
                     "Populate NEW-ARRAY with ELEMENT by mutation."
                     (dotimes index (array-total-size new-array)
                       (setf (row-major-aref new-array index) element)))
                   (populate-with-contents (dimensions contents)
                     "Populate NEW-ARRAY with CONTENTS by mutation."
                     (cond ((null dimensions)
                            (setf (row-major-aref new-array 0) contents))
                           ((null (rest dimensions))
                            (check-type contents sequence)
                            (assert (= (length contents) (first dimensions))) ; TODO Signals a better condition. [CONDITION]
                            (map nil
                                 (lambda (element)
                                   (check-type element element-type)
                                   (setf (row-major-aref new-array index) element)
                                   (incf index))
                                 contents)
                            (t
                             (check-type contents sequence)
                             (assert (= (length contents) (first dimensions))) ; TODO Signals a better condition. [CONDITION]
                             (map nil
                                  (lambda (element)
                                    (populate-with-contents (rest dimensions) element))
                                  contents)))))
                   (cond
                     (initial-element-p
                      (populate-with-element  canonicalized-dimensions initial-element))
                     (initial-contents-p
                      (populate-with-contents canonicalized-dimensions initial-contents))))))
        new-array)))
