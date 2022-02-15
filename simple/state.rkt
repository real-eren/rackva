#lang racket

(require "map.rkt")

(define return?-name "return?")
(define value-name "value")

;; creates a new state with return? = false and value = null
(define new-state (state-assign-var value-name
                                    null
                                    (state-assign-var return?-name
                                                      #f
                                                      map-empty)))

(define state-declare-var
  (lambda (var-name state)
    (map-insert var-name null state)))

(define state-assign-var
  (lambda (var-name value state)
    (map-insert var-name value state)))

(define state-var-declared?
  (lambda (state var-name)
    (map-result:has-value? (map-get var-name state))))

(define state-var-initialized?
  (lambda (state var-name)
    (null? state-var-value)))

(define state-var-value
  (lambda (state var-name)
    (map-result:get-value (map-get var-name state))))


(define state-set-return?
  (lambda (return? state)
    (state-assign-var return?-name return? state)))

(define state-return?
  (lambda (return? state)
    (state-var-value return?-name state)))


(define state-set-value
  (lambda (value state)
    (state-assign-var value-name value state)))

(define state-value
  (lambda (value state)
    (state-assign-var value-name value state)))

