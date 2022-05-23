#lang racket/base
(require racket/list)
;;;; Stack
;; "abstraction" for a stack
; a singly-linked list is already a stack,
; so this module's purpose is primarily to provide
; better names for common functions
(provide (prefix-out stack: (combine-out (all-defined-out)
                                         ormap
                                         foldl)))

(define push cons)
(define peek car)
(define pop cdr)

(define update-front
  (lambda (proc stack)
    (push (proc (peek stack))
          (pop stack))))

(define of list)

;; returns the first layer in the stack that satisfies a predicate
; false if absent
; searches top to bottom
(define firstf findf)


