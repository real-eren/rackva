#lang racket

(require "util/map.rkt")

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
                                  var-value
                                  bottom-frames)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Var-table is a stack of frames
;; a frame is a map of var bindings
;; a var binding associates a symbol with a box
;; beware, frames are mutable, thus var-table is too.
;; push a frame to enter a new scope
;; pop a frame to exit a scope
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Frame
(define new-frame map-empty)

(define frame-var-declared? map-contains?)

(define frame-var-box map-get)

(define frame-var-value
  (lambda (var-name frame)
    (unbox (frame-var-box var-name frame))))

;; assumes var is already declared
(define frame-assign-var
  (lambda (var-name val frame)
    (begin
      (set-box! (frame-var-box var-name frame) val)
      frame)))

(define frame-assign-box
  (lambda (var-name box frame)
    (map-put var-name box frame)))

(define frame-declare-var
  (lambda (var-name frame)
    (map-put var-name (box null) frame)))

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
    (push-frame (frame-assign-box var-name
                                  box
                                  (peek-frame var-table))
                (pop-frame var-table))))

; declares and assigns the var with the given value
(define declare-var-with-value
  (lambda (var-name value var-table)
    (push-frame (frame-assign-var var-name
                                  value
                                  (frame-declare-var var-name (peek-frame var-table)))
                (pop-frame var-table))))

; declares the var in the top frame
(define declare-var
  (lambda (var-name var-table)
    (push-frame (frame-declare-var var-name
                                   (peek-frame var-table))
                (pop-frame var-table))))

; find frame that declares it, update that frame
(define assign-var
  (lambda (var-name value var-table)
    (cond
      [(no-frames? var-table)                           (error "assigning to undeclared var")]
      [(frame-var-declared? var-name
                            (peek-frame var-table))     (push-frame (frame-assign-var var-name
                                                                                      value
                                                                                      (peek-frame var-table))
                                                                    (pop-frame var-table))]
      [else                                             (push-frame (peek-frame var-table)
                                                                    (assign-var var-name
                                                                                value
                                                                                (pop-frame var-table)))])))

(define var-declared-top-frame?
  (lambda (var-name var-table)
    (frame-var-declared? var-name (peek-frame var-table))))

(define var-declared?
  (lambda (var-name var-table)
    ;(ormap (lambda (frame) (frame-var-declared? var-name frame)) var-table)))
    (cond
      [(no-frames? var-table)                            #f]
      [(var-declared-top-frame? var-name var-table)      #t]
      [else                                              (var-declared? var-name
                                                                        (pop-frame var-table))])))


(define var-initialized?
  (lambda (var-name var-table)
    (not (null? (var-value var-name var-table)))))

;; returns the box bound to the var-name in the top frame that has it declared
(define var-box
  (lambda (var-name var-table)
    (cond
      [(no-frames? var-table)                        (error "failed to check for existence of variable before accessing")]
      [(frame-var-declared? var-name
                            (peek-frame var-table))  (frame-var-box var-name (peek-frame var-table))]
      [else                                          (var-box var-name
                                                              (pop-frame var-table))])))


; returns the value bound to the var-name in the top frame that has it declared
; use var-initialized? beforehand
; returns error if no such binding exists
(define var-value
  (lambda (var-name var-table)
    (unbox (var-box var-name var-table))))


;; returns the bottom n layers
(define bottom-frames
  (lambda (n var-table)
    (take-right var-table n)))
