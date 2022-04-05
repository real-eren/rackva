#lang racket

(require "var-table.rkt"
         "function-table.rkt")

(provide new-state
         (prefix-out state:
                     (combine-out height
                                  push-new-layer
                                  pop-layer
                                  bottom-layers

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
;; pair of a var-table and a function table

(define state-of
  (lambda (vars funs)
    (list vars funs)))

(define new-state (state-of new-var-table new-function-table))

(define vars first)
(define funs second)

;; height of the stack and function table
(define height
  (lambda (state)
    (length (vars state))))

;; State with the top scope removed from the stack and function table
(define pop-layer
  (lambda (state)
    (state-of (var-table:pop-frame (vars state))
              (function-table:pop-layer (funs state)))))

;; State with a blank frame added to the stack and function table
(define push-new-layer
  (lambda (state)
    (state-of (var-table:push-new-frame (vars state))
              (function-table:push-new-layer (funs state)))))


;; State with only the bottom n layers of the stack and function table
(define bottom-layers
  (lambda (n state)
    (state-of (var-table:bottom-frames n (vars state)
              (function-table:bottom-layers n (funs state))))))


;;;; var mappings


;; State with this varname declared in the current scope and initialized to this value
(define declare-var-with-box
  (lambda (name box state)
    (state-of (var-table:declare-var-with-box name box (vars state))
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
    (state-of (vars state)
              (function-table:declare-fun name params body scoper (funs state)))))


;; extract portions of a closure
(define closure:params function-table:closure:params)
(define closure:body function-table:closure:body)
(define closure:scoper function-table:closure:scoper)

