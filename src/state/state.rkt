#lang racket

(require "../util/map.rkt"
         "../util/stack.rkt"
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
                                  var-declared-current-scope?
                                  var-initialized?
                                  get-var-box
                                  get-var-value

                                  current-scope-has-fun?
                                  has-fun?
                                  get-function
                                  declare-fun

                                  has-class?
                                  get-class
                                  declare-class))
         function:params
         function:body
         function:scoper)


;;;; State
;; Entire (global + local) state of a program being interpreted
;; map w/ entries for
;; var-table
;; fun-table
;; stack-trace
;; classes
; TODO: add scope-stack to state, or modify stack-trace

(define $global-vars 'global-vars)
(define global-vars (map:getter $global-vars))

(define $local-vars 'local-vars)
(define local-vars (map:getter $local-vars))

(define $global-funs 'global-funs)
(define global-funs (map:getter $global-funs))

(define $local-funs 'local-funs)
(define local-funs (map:getter $local-funs))


; stack of the functions called
(define $stack-trace 'stack-trace)
(define stack-trace (map:getter $stack-trace))
(define $classes 'classes)
(define classes (map:getter $classes))

;; ex:
; (withv old-state
;        $local-funs  new-local-funs-table)
; (withf old-state
;        $global-vars  (curry function-table:declare function v1 v2 v3))
(define withv map:withv)
(define withf map:withf)
(define of map:of)

(define new-state (of $local-vars   (stack:of new-var-table)
                      $local-funs   (stack:of new-function-table)
                      $global-vars  new-var-table
                      $global-funs  new-function-table
                      $stack-trace  null))


;; State with the top scope removed from the stack and function table
(define pop-layer
  (lambda (state)
    (withf state
           $local-vars  stack:pop
           $local-funs  stack:pop)))

;; State with a blank frame added to the stack and function table
(define push-new-layer
  (lambda (state)
    (withf state
           $local-vars  (curry stack:push new-var-table)
           $local-funs  (curry stack:push new-function-table))))


;;;; function call stack-trace
(define with-stack-trace
  (lambda (stack-trace state)
    (withv state
           $stack-trace  stack-trace)))
(define push-stack-trace
  (lambda (fun-name state)
    (withf state
           $stack-trace  (curry cons fun-name))))

(define pop-stack-trace
  (lambda (state)
    (withf state
           $stack-trace  cdr)))


;; Given a state, creates a function that takes a state
; and returns the portion in-scope according to the original state
; the vars present in declare state
; the functions present in layers of invoke-state as high as declare-state
(define bottom-layers take-right) ; get the bottom/last layers of a function table
(define height length) ; count the number of layers in a function table

(define make-scoper
  (lambda (declare-state)
    (lambda (invoke-state)
      (of $local-vars  (bottom-layers (local-vars invoke-state) (height (local-vars declare-state)))
          $local-funs  (bottom-layers (local-funs invoke-state) (height (local-funs declare-state)))
          $stack-trace  (stack-trace invoke-state)))))

;;;; var mappings

;; State with this varname declared in the current scope and initialized to this value
(define declare-var-with-box
  (lambda (name box state)
    (withf state
           $local-vars  (curry var-table:declare-var-with-box name box))))

;; State with this varname declared in the current scope and initialized to this value
(define declare-var-with-value
  (lambda (name value state)
    (withf state
           $local-vars  (curry var-table:declare-var-with-value name value))))

;; State with this varname declared in the current scope
(define declare-var
  (lambda (name state)
    (withf state
           $local-vars (curry var-table:declare-var name))))

;; State with val assigned to this varname in the most recent scope containing such a name
(define assign-var
  (lambda (name val state)
    (withf state
           $local-vars  (curry var-table:assign-value name val))))

;; Is a variable with this name in scope?
(define var-declared?
  (lambda (name state)
    (var-table:var-declared? name (local-vars state))))

;; Is a variable with this name in scope in the current scope?
(define var-declared-current-scope?
  (lambda (name state)
    (var-table:var-declared? name (local-vars state))))

;; Is a variable with this name in scope and initialized?
(define var-initialized?
  (lambda (name state)
    (var-table:var-initialized? name (local-vars state))))

;; Get the box that backs this var
(define get-var-box
  (lambda (name state)
    (var-table:var-box name (local-vars state))))

;; get the current value of this var
(define get-var-value
  (lambda (name state)
    (var-table:var-value name (local-vars state))))


;;;; fun mappings

;; Is a function with this signature in the current scope (according to stack trace)
(define current-scope-has-fun?
  (lambda (name arg-list state)
    ; TODO has-fun && there exists a matching fun in the current scope
    ; if name is dotted
    ; if in top-level -> check global function table
    ; if in class body -> check class's function table
    ; check local function table
    ; if in instance context -> check class instance function table
    ; if in static context -> check class static function table
    (function-table:has-fun? name arg-list (stack:peek (local-funs state)))))

;; Is a function with this signature in scope?
(define has-fun?
  (lambda (name arg-list state)
    (ormap (curry function-table:has-fun? name arg-list)
           (local-funs state))))

; sweep through local, global, classes
(define get-all-funs
  (lambda (name state)
    (foldl append
           '()
           (map (curry function-table:get-all name) (local-funs state)))))
;; Get closure for the first function with this signature
; traverses scopes in this order:
; local -> instance -> static -> global
(define get-function
  (lambda (name arg-list state)
    (first (get-all-funs name state))))

;; State with this fun declared in the current scope
(define declare-fun
  (lambda (name params body scoper state)
    ; if in function, add to local
    ; if top level, global
    ; if in class body, add to 
    (withf state
           $local-funs  (curry stack:update-front
                               (curry function-table:declare-fun
                                      name
                                      params
                                      body
                                      scoper)))))

;;;; class

(define has-class?
  (lambda (class-name state)
    #f))

(define get-class
  (lambda (class-name state)
    null))

(define declare-class
  (lambda (class state)
    state))


;; extract portions of a closure
(define function:params (map:getter function:$params))
(define function:body (map:getter function:$body))
(define function:scoper (map:getter function:$scoper))

