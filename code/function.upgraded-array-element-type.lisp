;;;; Function UPGRADED-ARRAY-ELEMENT-TYPE
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_upgr_1.htm

(cl:in-package #:regalia)

;; NOTE This implementation is taken from
;; https://github.com/robert-strandh/SICL/blob/master/Code/Array/upgraded-array-element-type-defun.lisp
(defun upgraded-array-element-type (typespec &optional environment)
  (declare (ignore environment))
  (cond ((eq typespec 't) 't)
        ((subtypep typespec 'character) 'character)
        ((subtypep typespec '(complex double-float)) '(complex double-float))
        ((subtypep typespec '(complex single-float)) '(complex single-float))
        ((subtypep typespec 'double-float) 'double-float)
        ((subtypep typespec 'single-float) 'single-float)
        ((subtypep typespec 'bit) 'bit)
        ((subtypep typespec '(unsigned-byte 8)) '(unsigned-byte 8))
        ((subtypep typespec '(signed-byte 32)) '(signed-byte 32))
        ((subtypep typespec '(unsigned-byte 32)) '(unsigned-byte 32))
        ((subtypep typespec '(signed-byte 64)) '(signed-byte 64))
        ((subtypep typespec '(unsigned-byte 64)) '(unsigned-byte 64))
        (t 't)))
