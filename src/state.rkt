#lang racket

(require "var-table.rkt"
         "function-table.rkt")

(provide new-state
         (prefix-out state:
                     (combine-out push-new-var-frame
                                  pop-var-frame
                                  declare-var-with-value
                                  declare-var
                                  assign-var
                                  var-declared?
                                  var-declared-top-frame?
                                  var-initialized?
                                  get-var-box
                                  get-var-value
                                  bottom-var-frames
                                  
                                  push-new-funs-layer
                                  pop-funs-layer
                                  has-fun?
                                  get-closure
                                  declare-fun
                                  bottom-fun-layers))
         closure:params
         closure:body
         closure:scoper)


;;;; State
;; pair of a var-table and a function table

(define state-of
  (lambda (vars funs)
    (list vars funs)))

(define new-state (state-of new-var-table new-function-table))

(define vars first)
(define funs second)

;;;; var mappings

;; State with a blank stack frame added to the stack
(define push-new-var-frame
  (lambda (state)
    (state-of (var-table:push-new-frame (vars state))
              (funs state))))

;; State with the top frame of the stack removed
(define pop-var-frame
  (lambda (state)
    (state-of (var-table:pop-frame (vars state))
              (funs state))))

;; State with this varname declared in the current scope and initialized to this value
(define declare-var-with-value
  (lambda (name value state)
    (state-of (var-table:declare-var-with-value name value (vars state))
              (funs state))))

;; State with this varname declared in the current scope
(define declare-var
  (lambda (name state)
    (state-of (var-table:declare-var name (vars state))
              (funs state))))

;; State with 
(define assign-var
  (lambda (name val state)
    (state-of (var-table:assign-var name val (vars state))
              (funs state))))

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

;; State with the bottom n frames of the stack
(define bottom-var-frames
  (lambda (n state)
    (state-of (var-table:bottom-frames n (vars state))
              (funs state))))


;;;; fun mappings

;; State with a new blank layer of function bindings added
(define push-new-funs-layer
  (lambda (state)
    (state-of (vars state)
              (function-table:push-new-layer (funs state)))))

;; State with the top layer of function bindings removed
(define pop-funs-layer
  (lambda (state)
    (state-of (vars state)
              (function-table:pop-layer (funs state)))))

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
    (state-of (vars state)
              (function-table:declare-fun name params body scoper (funs state)))))

;; State with only the bottom n layers
(define bottom-fun-layers
  (lambda (n state)
    (state-of (vars state)
              (function-table:bottom-layers n (funs state)))))

;; extract portions of a closure
(define closure:params function-table:closure:params)
(define closure:body function-table:closure:body)
(define closure:scoper function-table:closure:scoper)

