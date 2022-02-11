#lang racket

(provide make-entry
         entry-key
         entry-value)
;;;;; map entry primitives ;;;;;

;; creates an entry pair from a key and a value
(define make-entry
  (lambda (key value)
    (cons key value)))

;; returns the key of an entry
(define entry-key
  (lambda (entry)
    (car entry)))

;; returns the value of an entry
(define entry-value
  (lambda (entry)
    (cdr entry)))
