#lang racket

(require "../util/map.rkt"
         "../util/stack.rkt"
         "var-table.rkt"
         "function.rkt"
         "function-table.rkt"
         "context.rkt"
         "class.rkt")

(provide new-state
         (prefix-out state:
                     (combine-out push-new-layer
                                  pop-layer
                                  
                                  context-stack
                                  with-context
                                  push-context
                                  pop-context
                                  
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
;; global vars and funs
;; local vars and funs    (layered)
;; context
;; classes


(define $global-vars 'global-vars)
(define global-vars (map:getter $global-vars))

(define $local-vars 'local-vars)
(define local-vars (map:getter $local-vars))

(define $global-funs 'global-funs)
(define global-funs (map:getter $global-funs))

(define $local-funs 'local-funs)
(define local-funs (map:getter $local-funs))

; stack of contexts. ex:
; (top-level) ; global var and fun defs
; (class-body 'A) ; class A declaration
; (fun static (foo closure)) -> (fun instance (bar closure)) ; during A::foo called B::bar
; 
(define $context-stack 'context-stack)
(define context-stack (map:getter $context-stack))
; map of class names to class closures
(define $classes 'classes)
(define classes (map:getter $classes))

;; ex:
; (withv old-state
;        $local-funs  new-local-funs-table)
; (withf old-state
;        $global-vars  (curry function-table:declare function name params body scoper))
(define withv map:withv)
(define withf map:withf)
(define of map:of)
(define update* map:update*)

(define new-state (of $local-vars   (stack:of new-var-table)
                      $local-funs   (stack:of new-function-table)
                      $global-vars  new-var-table
                      $global-funs  new-function-table
                      $context-stack  null))


;; State with the top scope removed from the stack and function table
(define pop-layer
  (lambda (state)
    (withf state
           $local-vars  stack:pop
           $local-funs  stack:pop)))

;; State with a blank frame added to the local var and fun tables
(define push-new-layer
  (lambda (state)
    (withf state
           $local-vars  (curry stack:push new-var-table)
           $local-funs  (curry stack:push new-function-table))))


;;;; stack of contexts - mostly fun calls
(define current-context (compose1 stack:peek context-stack))
(define with-context
  (lambda (context-stack state)
    (withv state
           $context-stack  context-stack)))

(define push-context
  (lambda (context state)
    (withf state
           $context-stack  (curry cons context))))

(define pop-context
  (lambda (state)
    (update* cdr state $context-stack)))

;; T/F whether the current context is of type `top-level`
(define top-level-context?
  (lambda (state)
    (eq? context:type:top-level (context:type (current-context state)))))

;; Function Closure / F whether the current context is of type `fun-call`
(define fun-call-context?
  (lambda (state)
    (context:fun-call:fun (current-context state))))


;; Given a state, creates a function that takes a state
; and returns the portion in-scope according to the original state
; the vars present in declare state
; the functions present in layers of invoke-state as high as declare-state
(define bottom-layers take-right) ; get the bottom/last layers of a function table
(define height length) ; count the number of layers in a function table

(define make-scoper
  (lambda (declare-state)
    (lambda (invoke-state)
      (withv invoke-state
             $local-vars  (bottom-layers (local-vars invoke-state) (height (local-vars declare-state)))
             $local-funs  (bottom-layers (local-funs invoke-state) (height (local-funs declare-state)))))))

;;;; var mappings

;; State with this varname declared in the current scope and initialized to this box
; top-level -> in global
; in function body -> local
(define declare-var-with-box
  (lambda (name box state)
    (withf state
           $local-vars  (curry stack:update-front (curry var-table:declare-var-with-box name box)))))

;; State with this varname declared in the current scope and initialized to this values
(define declare-var-with-value
  (lambda (name value state)
    (declare-var-with-box name (box value) state)))

;; State with this varname declared in the current scope
(define declare-var
  (lambda (name state)
    (declare-var-with-value name null state)))

;; State with val assigned to this varname in the most recent scope containing such a name
; assumes var has already been initialized
(define assign-var
  (lambda (name val state)
    (set-box! (get-var-box name state) val)
    state))


;; Get the box that backs the in-scope var with this name
; check if initialized first
(define get-var-box
  (lambda (name state)
    (cond
      [(ormap (curry var-table:var-box name) (local-vars state))]
      [else  (error "failed to check if var initialized before getting" name)])))
    ; check local,
    ; if instance context, check this' fields
    ; if class context, check class' fields
    ; if dotted
    ; check global,

;; get the current value of this var
(define get-var-value
  (lambda (name state)
    (unbox (get-var-box name state))))


;; Is a variable with this name in scope?
(define var-declared?
  (lambda (name state)
    (stack:ormap (curry var-table:var-declared? name) (local-vars state))))

;; Is a variable with this name in scope in the current scope?
(define var-declared-current-scope?
  (lambda (name state)
    (var-table:var-declared? name (stack:peek (local-vars state)))))

;; Is the in-scope variable with this name and initialized?
; check if declared first
(define var-initialized?
  (lambda (name state)
    (not (null? (get-var-value name state)))))


;;;; fun mappings

;; Is a function with this signature in the current scope (according to stack trace)
; used by interpreter to decide whether a declaration collides with an existing function
(define current-scope-has-fun?
  (lambda (name arg-list state)
    (cond
      ; TODO has-fun && there exists a matching fun in the current scope
      ; if name is dotted
      ; if in top-level -> check global function table
      [(eq? (current-context state)
            context:top-level)        (function-table:has-fun? name arg-list (global-funs state))]
      ; if in class body -> check class's function table
      ; check local function table
      [else                           (function-table:has-fun? name arg-list (stack:peek (local-funs state)))])))
; if in instance context -> check class instance function table
; if in static context -> check class static function table

;; Is a function with this signature in scope?
(define has-fun?
  (lambda (name arg-list state)
    (or (function-table:has-fun? name arg-list (global-funs state))
        (ormap (curry function-table:has-fun? name arg-list)
               (local-funs state)))))

;; sweep through local, global, classes
; primarily used by interpreter to display suggestions when reporting errors
; for calling an undefined function
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
    (cond
      [(ormap (curry function-table:get name arg-list) (local-funs state))]
      [(function-table:get name arg-list (global-funs state))]
      [else #f])))

;; State with this fun declared in the current scope
; only called for top-level or nested functions
(define declare-fun
  (lambda (name params body state)
    (let* ([ctxt       (current-context state)])
      (cond
        [(top-level-context? state)      (declare-fun-global name
                                                             params
                                                             body
                                                             state)]
        [(fun-call-context? state)  =>   (lambda (fc-fun)
                                           (declare-fun-local name
                                                              params
                                                              body
                                                              (function:scope fc-fun)
                                                              (function:class fc-fun)
                                                              state))]
        [else                            (declare-method name
                                                         params
                                                         body
                                                         ctxt
                                                         state)]))))

; global case of declare-fun
(define declare-fun-global
  (lambda (name params body state)
    (withf state
           $global-funs  (curry function-table:declare-fun
                                name
                                params
                                body
                                (make-scoper state)
                                function:scope:free
                                null))))

; local case of declare-fun
(define declare-fun-local
  (lambda (name params body scope class state)
    (withf state
           $local-funs  (curry stack:update-front
                               (curry function-table:declare-fun
                                      name
                                      params
                                      body
                                      (make-scoper state)
                                      scope
                                      class)))))

; method case of declare-fun
(define declare-method
  (lambda (name params body context state)
    (declare-method-impl name
                         params
                         body
                         (context:class-def-member:scope context)
                         (method-scope-to-table-key (context:class-def-member:scope context))
                         (context:class-def:name (current-context (pop-context state)))
                         state)))

(define method-scope-to-table-key
  (lambda (fun-scope)
    (cond
      [(eq? function:scope:static fun-scope)    class:$s-methods]
      [(eq? function:scope:instance fun-scope)  class:$i-methods]
      [(eq? function:scope:abstract fun-scope)  class:$a-methods])))

(define declare-method-impl
  (lambda (name params body fun-scope table-key class state)
    (update* (curry function-table:declare-fun
                    name
                    params
                    body
                    (make-scoper state)
                    fun-scope
                    class)
             state $classes class table-key)))



;;;; class

(define has-class?
  (lambda (class-name state)
    (map:in*? state $classes class-name)))

(define get-class
  (lambda (class-name state)
    (map:get* state $classes class-name)))

(define declare-class
  (lambda (class state)
    (map:put* class state $classes ('classname))))


;; extract portions of a closure
(define function:params (map:getter function:$params))
(define function:body (map:getter function:$body))
(define function:scoper (map:getter function:$scoper))


;;;;;;;; Unit Tests
(module+ test
  (require rackunit)
  (define s1 new-state)
  (define s2 (declare-var 'a s1))
  (check-true (var-declared? 'a s2))
  (check-false (var-initialized? 'a s2))
  
  (define s3 (assign-var 'a 3 s2))
  (check-true (var-initialized? 'a s3))
  (check-eq? 3 (get-var-value 'a s3))
  
  (define s4 (push-new-layer s3))
  (check-false (var-declared-current-scope? 'a s4))
  (check-eq? 3 (get-var-value 'a s4))
  (define s5 (declare-var-with-value 'a 7 s4))
  (check-eq? 7 (get-var-value 'a s5))
  (define s6 (pop-layer s5))
  (check-eq? 3 (get-var-value 'a s6))

  (define s7 (declare-var-with-box 'd (box 5)
                                   (declare-var 'c
                                                (declare-var-with-value 'b #T
                                                                        (push-new-layer s6)))))
  (check-eq? 5 (get-var-value 'd s7))
  (check-false (var-initialized? 'c s7))
  (check-eq? 5 (get-var-value 'd s7))
  
  (define s8 (assign-var 'd 10 (push-new-layer s7)))
  (check-eq? 10 (get-var-value 'd s8))

  ;; local fun lookup
  (let* ([s1      (push-context context:top-level new-state)]
         [fmn     'main] [fma     '()]
         [s2      (declare-fun fmn fma null s1)]
         [fm      (get-function fmn fma s2)]
         [fan     'a] [faa     '()]
         [fbn     'b] [fba     '()]
         [s3      (declare-fun fan faa null (push-context (context:of-fun-call fm) (push-new-layer s2)))]
         [s4      (declare-fun fbn fba null s3)]
         [s5      (pop-context (pop-layer s4))]
         )
    (check-true (has-fun? fmn fma s4))
    (check-true (has-fun? fan faa s4))
    (check-true (has-fun? fbn fba s4))

    (check-true (not (false? (get-function fan faa s4))))

    (check-false (has-fun? fan faa s5))
    (check-true (has-fun? fmn fma s5))
    )

  )