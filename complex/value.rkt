#lang racket

(require (combine-in "util/structures.rkt"
                     (only-in "util/my-lens.rkt"
                              getter)))

(provide (combine-out new-value
                      data-of
                      type-of))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; value
;; implementation of value used by a language
;; variables are assigned a 'value'
;; expressions evaluate to a 'value'
;; operators accept 'value's

;; this implementation has a 'data' and a 'type'
;; ex:  int x = 2
;; binds x to a value whose data is 2 and type is int

;; constructor that takes a 'data' and a 'type'
(define new-value
  (lambda (data type)
    (struc-2 data type)))

;; returns the 'data' of the given 'value'
(define data-of (getter e1-lens))

;; returns the 'type' of the given 'value'
(define type-of (getter e2-lens))
