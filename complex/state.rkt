#lang racket

(require (combine-in "util/structures.rkt"
                     (only-in "util/map.rkt"
                              map:empty)))

(provide (combine-out new-state
                      state-bindings-lens
                      default-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; represents the state of the program being interpreted

(define new-state struc-1)

(define state-bindings-lens e1-lens)

;; creates a default state
;; no bindings, return=false, value=undefined
(define default-state (new-state map:empty))
