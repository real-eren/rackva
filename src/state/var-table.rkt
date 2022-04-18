#lang racket

(require "../util/map.rkt")

(provide new-var-table
         (prefix-out var-table:
                     (combine-out push-new-frame
                                  pop-frame
                                  declare-var-with-box
                                  declare-var-with-value
                                  declare-var
                                  assign-var
                                  var-declared?
                                  var-declared-top-frame?
                                  var-initialized?
                                  var-box
                                  var-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Var-table is a stack of frames
;; a frame is a map of var bindings
;; a var binding associates a symbol with a box
;; beware, frames are mutable, thus var-table is too.
;; push a frame to enter a new scope
;; pop a frame to exit a scope
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Frame
(define new-frame map:empty)

(define frame-var-declared? map:contains?)

(define frame-var-box map:get)

(define frame-assign-box map:put)

(define frame-declare-var
  (lambda (var-name frame)
    (map:put var-name (box null) frame)))

;;;; stack operations
(define no-frames? null?)

(define push-frame cons)

(define push-new-frame
  (lambda (var-table)
    (push-frame new-frame var-table)))

; returns the top (most-recent) frame
(define peek-frame car)

; removes the top frame
(define pop-frame cdr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a new var-table with no var bindings
(define new-var-table (list new-frame))

; declares and assigns the var with the given box
(define declare-var-with-box
  (lambda (var-name box var-table)
    (list-update var-table 0 (curry frame-assign-box var-name box))))

; declares and assigns the var with the given value
(define declare-var-with-value
  (lambda (var-name value var-table)
    (declare-var-with-box var-name (box value) var-table)))

; declares the var in the top frame
(define declare-var
  (lambda (var-name var-table)
    (list-update var-table 0 (curry frame-declare-var var-name))))

; find frame that declares it, update that frame
(define assign-var
  (lambda (var-name value var-table)
    (begin
      (set-box! (var-box var-name var-table) value)
      var-table)))

(define var-declared-top-frame?
  (lambda (var-name var-table)
    (frame-var-declared? var-name (peek-frame var-table))))

(define var-declared?
  (lambda (var-name var-table)
    (ormap (curry frame-var-declared? var-name) var-table)))

(define var-initialized?
  (lambda (var-name var-table)
    (not (null? (var-value var-name var-table)))))

;; returns the box bound to the var-name in the top frame that has it declared
; use var-initialized? beforehand, error if no such binding exists
(define var-box
  (lambda (var-name var-table)
    (frame-var-box var-name (findf (curry frame-var-declared? var-name) var-table))))

; returns the value bound to the var-name in the top frame that has it declared
(define var-value
  (lambda (var-name var-table)
    (unbox (var-box var-name var-table))))

