#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; STATE ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; creates a default state
;; no bindings, return=false, value=undefined
(define init-state
  (lambda (init-map)
    ('((init-map)      ;; vars name:value bindings
       (init-map)))))  ;; registers name:value bindings

;; retrieves the var bindings of the state
(define bindings-lens
  (lambda (state)
    (car state)))

;; retrieves the registers of the state
(define registers-lens
  (lambda (state)
    (cadr state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
