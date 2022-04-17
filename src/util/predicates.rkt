#lang racket
(provide join||
         join&&)

;;;; collection of higher-order functions for composing predicates

;; takes a sequence of 1-arg predicates and produces a predicate
;; that returns true iff any predicate would return true.
;; behaves as if the predicates were joined by short-circuit or
; ex: (filter (any 
(define join||
  (lambda preds
    (lambda (v)
      (ormap (lambda (p) (p v)) preds))))

;; takes a sequence of 1-arg predicates and produces a 1-arg predicate
;; that returns true iff any predicate would return true.
;; behaves as if the predicates were joined by short-circuit and
(define join&&
  (lambda preds
    (lambda (v)
      (andmap (lambda (p) (p v)) preds))))
