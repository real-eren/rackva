#lang racket

(require (combine-in "util/structures.rkt"
                     (only-in "util/my-lens.rkt"
                              getter
                              lens-compose)
                     (only-in "state.rkt"
                              default-state)
                     (only-in "registers.rkt"
                              default-registers
                              registers-return?-lens
                              registers-value-lens)))

(provide (combine-out new-context
                      default-context
                      context-state-lens
                      context-registers-lens
                      context-return?
                      context-current-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; contains state and registers

(define new-context struc-2)

;; creates a default context
;; no bindings, return=false, value=undefined
(define default-context (new-context default-state default-registers))

(define context-state-lens e1-lens)

(define context-registers-lens e2-lens)

; returns whether the given 'context' indicates to return
(define context-return?
  (getter (lens-compose (list registers-return?-lens context-registers-lens))))

; returns the current-value of the context
(define context-current-value
  (getter (lens-compose (list registers-value-lens context-registers-lens))))
