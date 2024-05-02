;;;; Function ADJUST-ARRAY
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_adjust.htm

(cl:in-package #:regalia)

;;; Code below taken from SICL

(defun copy-elements (from-array to-array dimensions)
  (let ((rank (length dimensions)))
    (case rank
      (0
       (setf (aref to-array) (aref from-array)))
      (1
       (loop for index from 0 below (first dimensions)
             do (setf (aref to-array index)
                      (aref from-array index))))
      (2
       (loop for index1 from 0 below (first dimensions)
             do (loop for index2 from 0 below (second dimensions)
                      do (setf (aref to-array index1)
                               (aref from-array index2)))))
      (otherwise
       (labels ((aux (indices-so-far remaining-dimensions)
                  (if (null remaining-dimensions)
                      (apply #'(setf aref)
                             (apply #'aref from-array indices-so-far)
                             to-array indices-so-far)
                      (loop for index from 0 below (first remaining-dimensions)
                            do (aux (cons index indices-so-far)
                                    (rest remaining-dimensions))))))
         (aux '() (reverse dimensions)))))))

(defun adjust-array
    (array
     new-dimensions
     &rest keyword-arguments
     &key
       (element-type (array-element-type array))
       (initial-element nil initial-element-p)
       (initial-contents nil initial-contents-p)
       fill-pointer
       (displaced-to nil displaced-to-p)
       (displaced-index-offset nil displaced-index-offset-p))


  ;; NOTE SPEC: initial-element must not be supplied if either
  ;; initial-contents or displaced-to is supplied.
  ;;
  ;; NOTE SPEC: initial-contents must not be supplied if either
  ;; initial-element or displaced-to is given.
  ;;
  ;; NOTE SPEC: initial-elements and initial-contents must not be supplied if
  ;; displaced-to is supplied.
  ;;
  ;; NOTE: In short, at most one among initial-element, initial-contents, and
  ;; displaced-to can be supplied.
  (when (or (and initial-element-p (or initial-contents-p displaced-to-p))
            (and initial-contents-p (or initial-element-p displaced-to-p)))
    ;; TODO Write a better condition signal. [CONDITION]
    (error))

  (setf element-type (upgraded-array-element-type element-type))

  (let* ((canonicalized-dimensions
           (if (listp new-dimensions) new-dimensions (list new-dimensions)))
         (old-dimensions (array-dimensions array))
         (old-dimensions
           (if (listp old-dimensions) old-dimensions (list old-dimensions))))
    ;; NOTE SPEC: The result is an array of the same type and rank as array.
    ;;
    ;; TODO How to enforce the "type" part?
    (unless (= (length old-dimensions) (length canonicalized-dimensions))
      ;; TODO Provide this condition.
      (error 'attempt-to-change-the-rank
             :array array
             :old-dimensions old-dimensions
             :new-dimensions canonicalized-dimensions))

    ;; NOTE SPEC: Element-type specifies the type of the elements of the
    ;; resulting array. If element-type is supplied, the consequences are
    ;; unspecified if the upgraded array element type of element-type is not
    ;; the same as the actual array element type of array. TODO

    (let ((new-array
            (if (= (length canonicalized-dimensions) 1)
                (let ((resulting-fill-pointer
                        (if (null fill-pointer)
                            ;; The standard says that if FILL-POINTER is NIL, then
                            ;; the fill pointer of the array should be left as it
                            ;; is.
                            (vector-fill-pointer array)
                            fill-pointer)))
                  (apply #'make-array
                         new-dimensions
                         :element-type element-type
                         :fill-pointer resulting-fill-pointer
                         keyword-arguments))
                (apply #'make-array
                       new-dimensions
                       :element-type element-type
                       keyword-arguments))))
      (unless initial-contents-p
        (copy-elements
         array new-array (mapcar #'min old-dimensions canonicalized-dimensions)))

      ;; TODO What to do with this?
      ;; (progn (sicl-primop:set-rack array (sicl-primop:rack new-array)) nil)
      ))
  array)
