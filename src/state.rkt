#lang racket

(require "util/map.rkt")

(provide new-state
         (prefix-out state-
                     (combine-out push-new-frame
                                  pop-frame
                                  declare-var
                                  assign-var
                                  var-declared?
                                  var-declared-top-frame?
                                  var-initialized?
                                  var-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; State is a stack of frames
;; a frame is a map of var bindings
;; push a frame to enter a new scope
;; pop a frame to exit a scope
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Frame
(define new-frame map-empty)

(define frame-var-declared? map-contains?)

(define frame-var-initialized?
  (lambda (var-name frame)
    (not (null? var-value var-name frame))))

(define frame-var-value map-get)

(define frame-assign-var map-put)

(define frame-declare-var
  (lambda (var-name frame)
    (frame-assign-var var-name null frame)))

;;;; State stack operations
(define no-frames? null?)

(define push-frame cons)

(define push-new-frame
  (lambda (state)
    (push-frame new-frame state)))

; returns the top (most-recent) frame
(define peek-frame car)

; removes the top frame
(define pop-frame cdr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; creates a new state with no var bindings
(define new-state (list (frame-declare-var 'return-value new-frame)))

; declares the var in the top frame
(define declare-var
  (lambda (var-name state)
    (push-frame (frame-declare-var var-name
                                   (peek-frame state))
                (pop-frame state))))

; find frame that declares it, update that frame
(define assign-var
  (lambda (var-name value state)
    (cond
      [(no-frames? state)                           (error "assigning to undeclared var")]
      [(frame-var-declared? var-name
                            (peek-frame state))     (push-frame (frame-assign-var var-name
                                                                                  value
                                                                                  (peek-frame state))
                                                                (pop-frame state))]
      [else                                         (push-frame (peek-frame state)
                                                                (assign-var var-name
                                                                            value
                                                                            (pop-frame state)))])))

(define var-declared-top-frame?
  (lambda (var-name state)
    (frame-var-declared? var-name (peek-frame state))))

(define var-declared?
  (lambda (var-name state)
    ;(ormap (lambda (frame) (frame-var-declared? var-name frame)) state)))
    (cond
      [(no-frames? state)                            #f]
      [(var-declared-top-frame? var-name state)      #t]
      [else                                          (var-declared? var-name
                                                                    (pop-frame state))])))


(define var-initialized?
  (lambda (var-name state)
    (not (null? (var-value var-name state)))))

; returns the value bound to the var-name in the top frame that has it declared
; use var-initialized? beforehand
; returns error if no such binding exists
(define var-value
  (lambda (var-name state)
    (cond
      [(no-frames? state)                            (error "")]
      [(frame-var-declared? var-name
                            (peek-frame state))      (frame-var-value var-name
                                                                      (peek-frame state))]
      [else                                          (var-value var-name
                                                                (pop-frame state))])))
