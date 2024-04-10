;;;; Accessor AREF
;;;; https://www.lispworks.com/documentation/HyperSpec/Body/f_aref.htm

(cl:in-package #:regalia)

;; TODO Use array-row-major-index and row-major-aref (provided by host) to implement aref:
;; (aref array i1 i2 ...) == (row-major-aref array (array-row-major-index array i1 i2 ..))

;; TODO Write the writer too, by using the writer of row-major-aref.
