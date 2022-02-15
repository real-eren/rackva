#lang racket
(require "state.rkt")

(provide interpret
         Mstate)

(define interpret
  (lambda (statements)
    (print-result (Mstate (cdr statements)
                          (Mstate (car statements)
                                  new-state)))))


(define Mstate
  (lambda (expr state)
    (cond
      [(state-return? state)         state] ; exit early on return
      )))


;; takes a state and prints the output
(define print-result
  (lambda (state)
    (cond
      [(not (state-return? state))          (error "Missing return statement")]
      [(eq? #t (state-value state))         (print "true")]
      [(eq? #f (state-value state))         (print "false")]
      [(number? (state-value state))        (print (state-value state))]
      [else                                 (error "returned a value, but not a bool or int")])))

