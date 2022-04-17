#lang racket

(require "../util/map.rkt"
         "var-table.rkt"
         "function-table.rkt")

(provide new-state
         (prefix-out state:
                     (combine-out push-new-layer
                                  pop-layer
                                  push-stack-trace
                                  pop-stack-trace

                                  stack-trace
                                  with-stack-trace

                                  make-scoper

                                  declare-var-with-box
                                  declare-var-with-value
                                  declare-var
                                  assign-var
                                  var-declared?
                                  var-declared-top-frame?
                                  var-initialized?
                                  get-var-box
                                  get-var-value

                                  has-fun?
                                  get-closure
                                  declare-fun))
         closure:params
         closure:body
         closure:scoper)


;;;; State
;; Entire (global + local) state of a program being interpreted
;; map w/ entries for
;; var-table
;; fun-table
;; stack-trace

(define vars-key 'vars)
(define vars (map:getter vars-key))
(define funs-key 'funs)
(define funs (map:getter funs-key))
; stack of the functions called
(define stack-trace-key 'stack-trace)
(define stack-trace (map:getter stack-trace-key))

(define state-of
  (lambda ([state new-state]
           #:vars [vars (vars state)]
           #:funs [funs (funs state)]
           #:stack-trace [stack-trace (stack-trace state)])
    (map:from-interlaced-entries
     vars-key vars
     funs-key funs
     stack-trace-key stack-trace)))

(define new-state (state-of map:empty
                            #:vars new-var-table
                            #:funs new-function-table
                            #:stack-trace null))


;; State with the top scope removed from the stack and function table
(define pop-layer
  (lambda (state)
    (state-of state
              #:vars (var-table:pop-frame (vars state))
              #:funs (function-table:pop-layer (funs state)))))

;; State with a blank frame added to the stack and function table
(define push-new-layer
  (lambda (state)
    (state-of state
              #:vars (var-table:push-new-frame (vars state))
              #:funs (function-table:push-new-layer (funs state)))))


;;;; function call stack-trace
(define with-stack-trace
  (lambda (stack-trace state)
    (state-of state
              #:stack-trace stack-trace)))
(define push-stack-trace
  (lambda (fun-name state)
    (state-of state
              #:stack-trace (cons fun-name (stack-trace state)))))

(define pop-stack-trace
  (lambda (state)
    (state-of state
              #:stack-trace (cdr (stack-trace state)))))


;; Given a state, creates a function that takes a state
; and returns the portion in-scope according to the original state
; the vars present in declare state
; the functions present in layers of invoke-state as high as declare-state
(define bottom-layers take-right) ; get the bottom/last layers of a function table
(define height length) ; count the number of layers in a function table

(define make-scoper
  (lambda (declare-state)
    (lambda (invoke-state)
      (state-of #:vars (bottom-layers (vars invoke-state) (height (vars declare-state)))
                #:funs (bottom-layers (funs invoke-state) (height (funs declare-state)))
                #:stack-trace (stack-trace invoke-state)))))

;;;; var mappings

;; State with this varname declared in the current scope and initialized to this value
(define declare-var-with-box
  (lambda (name box state)
    (state-of state
              #:vars (var-table:declare-var-with-box name box (vars state)))))

;; State with this varname declared in the current scope and initialized to this value
(define declare-var-with-value
  (lambda (name value state)
    (state-of state
              #:vars (var-table:declare-var-with-value name value (vars state)))))

;; State with this varname declared in the current scope
(define declare-var
  (lambda (name state)
    (state-of state
              #:vars (var-table:declare-var name (vars state)))))

;; State with val assigned to this varname in the most recent scope containing such a name
(define assign-var
  (lambda (name val state)
    (state-of state
              #:vars (var-table:assign-var name val (vars state)))))

;; Is a variable with this name in scope?
(define var-declared?
  (lambda (name state)
    (var-table:var-declared? name (vars state))))

;; Is a variable with this name in scope in the current scope?
(define var-declared-top-frame?
  (lambda (name state)
    (var-table:var-declared-top-frame? name (vars state))))

;; Is a variable with this name in scope and initialized?
(define var-initialized?
  (lambda (name state)
    (var-table:var-initialized? name (vars state))))

;; Get the box that backs this var
(define get-var-box
  (lambda (name state)
    (var-table:var-box name (vars state))))

;; get the current value of this var
(define get-var-value
  (lambda (name state)
    (var-table:var-value name (vars state))))


;;;; fun mappings

;; Is a function with this name in scope?
(define has-fun?
  (lambda (name state)
    (function-table:has-fun? name (funs state))))

;; Get closure for the first (layer-wise) function with this name
(define get-closure
  (lambda (name state)
    (function-table:get-closure name (funs state))))

;; State with this fun declared in the current layer
(define declare-fun
  (lambda (name params body scoper state)
    (state-of state
              #:funs (function-table:declare-fun name
                                                 params
                                                 body
                                                 scoper
                                                 (funs state)))))



;; extract portions of a closure
(define closure:params function-table:closure:params)
(define closure:body function-table:closure:body)
(define closure:scoper function-table:closure:scoper)
