#lang racket

(provide (prefix-out entry: (combine-out of
                                         key
                                         value)))

;;;;; map entry primitives ;;;;;

;; creates an entry pair from a key and a value
(define of cons)

;; returns the key of an entry
(define key car)

;; returns the value of an entry
(define value cdr)
