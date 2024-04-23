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
                     (adjustable)
                     (fill-pointer)
                     (displaced-to nil displaced-to-p)
                     (displaced-index-offset nil displaced-index-offset-p))

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
              ;; FIXME Rank may be zero, in each case the resulting array is
              ;; 0-dimensional, and can hold one object.
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


        (if initial-element-p
            (progn
              ;; NOTE SPEC: If initial-element is supplied, it is used to
              ;; initialize each element of new-array.
              ;;
              ;; TODO

              ;; NOTE SPEC: If initial-element is supplied, it must be of the
              ;; type given by element-type.
              (check-type initial-element element-type)

              ;; NOTE SPEC: initial-element cannot be supplied if either the
              ;; :initial-contents option is supplied or displaced-to is
              ;; non-nil.
              (when (or initial-contents-p displaced-to)
                ;; TODO Write a better condition signal.
                (error))

              )
            (progn
              ;; NOTE SPEC: If initial-element is not supplied, the
              ;; consequences of later reading an uninitialized element of
              ;; new-array are undefined unless either initial-contents is
              ;; supplied or displaced-to is non-nil. TODO Question. What does
              ;; this mean?
              ;;
              ;; TODO
              ))

        (if initial-contents-p
            (progn
              ;; NOTE SPEC: initial-contents is composed of a nested structure
              ;; of sequences. TODO

              ;; NOTE SPEC: The numbers of levels in the structure must equal
              ;; the rank of array. TODO

              ;; NOTE SPEC: Each leaf of the nested structure must be of the
              ;; type given by element-type. TODO



              ;; NOTE SPEC: If array is zero-dimensional, then
              ;; initial-contents specifies the single element.

              ;; NOTE SPEC: Otherwise, initial-contents must be a sequence
              ;; whose length is equal to the first dimension; each element
              ;; must be a nested structure for an array whose dimensions are
              ;; the remaining dimensions, and so on.

              ;; NOTE SPEC: Initial-contents cannot be supplied if either
              ;; initial-element is supplied or displaced-to is non-nil.
              (when (or initial-element-p displaced-to)
                ;; TODO Write a better condition signal.
                (error))

              ;; NOTE SPEC: If initial-contents is not supplied, the
              ;; consequences of later reading an uninitialized element of
              ;; new-array are undefined unless either initial-element is
              ;; supplied or displaced-to is non-nil. TODO Question. What does
              ;; this mean?
              )

            (progn
              ;; TODO
              ))

        (if adjustable
            (progn
              ;; NOTE SPEC: If adjustable is non-nil, the array is expressly
              ;; adjustable (and so actually adjustable);
              )
            (progn
              ;; NOTE SPEC: otherwise, the array is not expressly adjustable
              ;; (and it is implementation-dependent whether the array is
              ;; actually adjustable).
              ))

        (cond ((not (null fill-pointer))
               ;; NOTE SPEC: If fill-pointer is non-nil, the array must be
               ;; one-dimensional; that is, the array must be a vector. TODO
               )
              ((eq t fill-pointer)
               ;; NOTE SPEC: If fill-pointer is t, the length of the vector is
               ;; used to initialize the fill pointer. TODO
               )
              ((integerp fill-pointer)
               ;; NOTE SPEC: If fill-pointer is an integer, it becomes the initial
               ;; fill pointer for the vector.
               ))

        (if displaced-to
            (progn
              ;; NOTE SPEC: If displaced-to is non-nil, make-array will create a displaced
              ;; array and displaced-to is the target of that displaced array.

              ;; NOTE SPEC: [If displaced-to is non-nil], the consequences are
              ;; undefined if the actual array element type of displaced-to is not
              ;; type equivalent to the actual array element type of the array being
              ;; created.
              )
            (progn
              ;; NOTE SPEC: If displaced-to is nil, the array is not a displaced
              ;; array.
              ))

        ;; TODO
        ;; NOTE DISPLACED-INDEX-OFFSET
        ;;
        ;; The displaced-index-offset is made to be the index offset of the
        ;; array. TODO
        ;;
        ;; When an array A is given as the :displaced-to argument to
        ;; make-array when creating array B, then array B is said to be
        ;; displaced to array A. TODO
        ;;
        ;; The total number of elements in an array, called the total size of
        ;; the array, is calculated as the product of all the dimensions. TODO
        ;;
        ;; It is required that the total size of A be no smaller than the sum
        ;; of the total size of B plus the offset n supplied by the
        ;; displaced-index-offset. TODO
        ;;
        ;; The effect of displacing is that array B does not have any elements
        ;; of its own, but instead maps accesses to itself into accesses to
        ;; array A. TODO
        ;;
        ;; The mapping treats both arrays as if they were one-dimensional by
        ;; taking the elements in row-major order, and then maps an access to
        ;; element k of array B to an access to element k+n of array A. TODO


        ;; FIXME The logic that spec specifies for handling keyword arguments
        ;; is not as simple. Write %compute-branch for making the decision.

        (when initial-contents-p ; This condition should be replaced by the output of %compute-branch
          (let ((index 0))
            (labels ((init (dimensions contents)
                       "Populate NEW-ARRAY by mutation."
                       (cond ((null dimensions)
                              (setf (row-major-aref new-array 0) contents))
                             ((null (rest dimensions))
                              (check-type contents sequence)
                              ;; TODO Isn't there a loop keyword that works
                              ;; for sequences in general? TODO ANSWER: Use
                              ;; #'map with RESULT-TYPE NIL:
                              ;; https://novaspec.org/cl/f_map
                              ;;
                              ;; TODO CHECK: A sequence is either a list of an
                              ;; "ACROSSABLE"? FIXME Answer is no! SPEC: "The
                              ;; types vector and the type list are disjoint
                              ;; subtypes of type sequence, but are not
                              ;; necessarily an exhaustive partition of
                              ;; sequence." in
                              ;; https://novaspec.org/cl/t_sequence. FIXME And
                              ;; answer is also yes! SPEC: "A sequence is an
                              ;; ordered collection of elements, implemented
                              ;; as either a vector or a list."
                              ;; https://novaspec.org/cl/17_1_Sequence_Concepts
                              ;; NOTE This is a contradiction in the SPEC!
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
                              (check-type contents sequence)
                              ;; TODO Isn't there a loop keyword that works
                              ;; for sequences in general? TODO ANSWER: Use
                              ;; #'map with RESULT-TYPE NIL:
                              ;; https://novaspec.org/cl/f_map
                              ;;
                              ;; TODO CHECK: A sequence is either a list of an
                              ;; "ACROSSABLE"? FIXME Answer is no! SPEC: "The
                              ;; types vector and the type list are disjoint
                              ;; subtypes of type sequence, but are not
                              ;; necessarily an exhaustive partition of
                              ;; sequence." in
                              ;; https://novaspec.org/cl/t_sequence. FIXME And
                              ;; answer is also yes! SPEC: "A sequence is an
                              ;; ordered collection of elements, implemented
                              ;; as either a vector or a list."
                              ;; https://novaspec.org/cl/17_1_Sequence_Concepts
                              ;; NOTE This is a contradiction in the SPEC!
                              (if (listp contents)
                                  (loop for element in contents
                                        repeat (first dimensions)
                                        do (init (rest dimensions) element))
                                  (loop for element across contents
                                        repeat (first dimensions)
                                        do (init (rest dimensions) element)))))))
              (init canonicalized-dimensions initial-contents))))

        new-array))))
