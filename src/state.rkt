#lang racket

(require "map.rkt")

(provide new-state
         (prefix-out state-
                     (combine-out declare-var
                                  assign-var
                                  var-declared?
                                  var-initialized?
                                  var-value
                                  return?
                                  set-return-value
                                  get-return-value)))


;; creates a new state with no var bindings
(define new-state map-empty)


(define declare-var
  (lambda (var-name state)
    (map-put var-name null state)))


(define assign-var
  (lambda (var-name value state)
    (map-put var-name value state)))


(define var-declared?
  (lambda (var-name state)
    (map-contains? var-name state)))


(define var-initialized?
  (lambda (var-name state)
    (not (null? var-value))))


(define var-value
  (lambda (var-name state)
    (map-get var-name state)))


(define return-value-name "return-value")


(define return?
  (lambda (state)
    (map-contains? return-value-name state)))


(define get-return-value
  (lambda (state)
    (var-value return-value-name state)))


(define set-return-value
  (lambda (value state)
    (assign-var return-value-name value state)))