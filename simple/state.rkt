#lang racket

(require "map.rkt")

(provide new-state
         (prefix-out state-
                     (combine-out declare-var
                                  assign-var
                                  var-declared?
                                  var-initialized?
                                  var-value
                                  set-return?
                                  return?
                                  set-return-value
                                  get-return-value)))
(define return?-name "return?")
(define value-name "value")

(define declare-var
  (lambda (var-name state)
    (map-insert var-name null state)))

(define assign-var
  (lambda (var-name value state)
    (map-insert var-name value state)))

(define var-declared?
  (lambda (var-name state)
    (map-contains? var-name state)))

(define var-initialized?
  (lambda (var-name state)
    (null? var-value)))

(define var-value
  (lambda (var-name state)
    (map-result:get-value (map-get var-name state))))


;; creates a new state with return? = false and value = null
(define new-state (assign-var value-name
                              null
                              (assign-var return?-name
                                          #f
                                          map-empty)))


(define set-return?
  (lambda (return? state)
    (assign-var return?-name return? state)))

(define return?
  (lambda (state)
    (var-value return?-name state)))


(define get-return-value
  (lambda (state)
    (var-value value-name state)))

(define set-return-value
  (lambda (value state)
    (assign-var value-name value state)))
