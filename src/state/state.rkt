#lang racket

(require "../util/map.rkt"
         "../util/stack.rkt"
         "../util/predicates.rkt"
         "var-table.rkt"
         "instance.rkt"
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
                                  push-top-level-context
                                  push-fun-call-context
                                  pop-context
                                  instance-context?
                                  formatted-stack-trace

                                  set-static-scope
                                  set-instance-scope
                                  set-this-scope
                                  set-super-scope
                                  restore-scope

                                  declare-var-with-box
                                  declare-var-with-value
                                  declare-var
                                  assign-var
                                  var-declared?
                                  var-already-declared?
                                  var-initialized?
                                  get-var-box
                                  get-var-value

                                  fun-already-declared?
                                  has-fun?
                                  get-function
                                  declare-fun

                                  declare-class
                                  has-class?
                                  get-class
                                  current-type-has-parent?
                                  get-parent-name
                                  get-parents-abstract-methods
                                  class-declares-inst-or-abst-method?
                                  class-get-inst-method
                                  declare-method
                                  method-already-declared?

                                  get-zero-init-instance
                                  get-init
                                  get-constructor
                                  declare-inst-field
                                  declare-init
                                  declare-constructor
                                  
                                  end-class-decl)))


;;;; State
;; Entire (global + local) state of a program being interpreted
;; map w/ entries for
;; global vars and funs
;; local vars and funs    (layered)
;; context
;; classes

; general assumptions:
; 1) context stack is non-empty before state is used


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
(define $context-stack 'context-stack)
(define context-stack (map:getter $context-stack))

;; current class. Name / #F. affects what bindings are in scope
(define $current-type 'current-type)
(define current-type (map:getter $current-type))

;; current instance. instance / #F
(define $this 'this)
(define this (map:getter $this))

;; flag that affects lookup. if true, limited to bindings available to instance and type
(define $dotted 'dotted)
(define dotted? (map:getter $dotted))
(define $super 'super)
(define super (map:getter $super))

; map of class names to class closures
(define $classes 'classes)
(define classes (map:getter $classes))

;; ex:
; (withv old-state
;        $local-funs   new-local-funs-table)
;        $global-funs  new-global-funs-table)
; (withf old-state
;        $global-vars  (curry fun-table:declare function name params body scoper))
(define withv map:withv)
(define withf map:withf)
(define of map:of)
(define update* map:update*) ; nested map accessors
(define put* map:put*)
(define get* map:get*)

(define new-state (of $local-vars   null
                      $local-funs   null
                      $global-vars  new-var-table
                      $global-funs  new-function-table
                      $classes      map:empty
                      $current-type   #F
                      $this           #F
                      $dotted         #F
                      $super          #F
                      $context-stack  null))


;; State with the top scope removed from the stack and function table
; assumption: non-empty stacks for local var and fun tables
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


;;;; stack of contexts - top-level, fun-call, constructors, class-defs
(define with-context
  (lambda (context-stack state)
    (withv state
           $context-stack  context-stack)))

; assumption: existant & non-empty entry for context-stack
(define current-context (compose1 stack:peek context-stack))

; assumption: pre-existing entry for context-stack
(define push-top-level-context
  (lambda (state)
    (push-context context:top-level state)))

(define push-fun-call-context
  (lambda (fun state)
    (push-context (context:of-fun-call fun) state)))

(define push-context
  (lambda (context state)
    (withf state
           $context-stack  (curry stack:push context))))

; assumption: non-empty context-stack
(define pop-context
  (lambda (state)
    (update* stack:pop
             state $context-stack)))

;; T/F whether the current context is of type `top-level`
(define top-level-context?
  (lambda (state)
    (eq? context:type:top-level (context:type (current-context state)))))

;; instance / F 
(define instance-context?
  (lambda (state)
    (this state)))

;; Whether declarations should go to the local tables
; blocks, fun-calls
; Assumptions:
; 1) local-var-tables stack is empty when not in a local context
; 2) a layer is always pushed when calling a function, entering a block
(define local-context?
  (lambda (state)
    (< 0 (height (local-vars state)))))

;; Function Closure / F whether the current context is of type `fun-call`
(define fun-call-context?
  (lambda (state)
    (context:fun-call:fun (current-context state))))

;; class-name/F whether the current context is of type `class-def`
(define class-def-context?
  (lambda (state)
    (context:class-def:name (current-context state))))



;; retrieves the context-stack from state and returns it in a form fit for output
(define formatted-stack-trace
  (lambda (state)
    (if (empty? (context-stack state))
        ""
        (string-join (map (curry format "~a") (reverse (context-stack state)))
                     " -> "
                     #:before-first "stack trace: "))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define bottom-layers take-right) ; get the bottom/last layers of a function table
(define height length) ; count the number of layers in a function table


;; Given a state, creates a function that takes a state
; and returns the portion in-scope according to the original state
; assumptions:
; 1) invoke-state has at least as many layers as declare-state
; 2) as many local layers as are in scope during declare-state belong in scope
; 3) all of invoke state's classes and global tables belong in scope
; why height and not copy? to capture bindings declared later in the same layer
(define make-scoper
  (lambda (declare-state class instance-context?)
    (lambda (invoke-state)
      (withv invoke-state
             $local-vars  (bottom-layers (local-vars invoke-state) (height (local-vars declare-state)))
             $local-funs  (bottom-layers (local-funs invoke-state) (height (local-funs declare-state)))
             $current-type  class
             $this          (if instance-context? (this invoke-state) #F)
             $dotted        #F))))

;;;; Used by `Mstate-dot` to make state perform the correct lookups
(define set-static-scope
  (lambda (class-name state)
    (withv state
           $current-type  class-name
           $this          #F
           $super         #F
           $dotted        #T)))

(define set-instance-scope
  (lambda (this state)
    (withv state
           $current-type  (instance:class this)
           $this          this
           $super         #F
           $dotted        #T)))

(define set-this-scope
  (lambda (state)
    (withv state
           $super         #F
           $dotted        #T)))

(define set-super-scope
  (lambda (state)
    (withv state
           $current-type  (get-parent-name (current-type state) state)
           $super         #T
           $dotted        #T)))

;; copies scope related data from src onto dest
(define restore-scope
  (lambda (#:src old-state
           #:dest new-state)
    (withv new-state
           $current-type  (current-type old-state)
           $dotted        #F
           $this          (this old-state))))





;;;;;;;;;;;;;;;; VARIABLES

;; State with this varname declared in the current scope and initialized to this box
; top-level -> in global
; in local (such as function body) -> local
(define declare-var-with-box
  (lambda (name box state)
    (cond
      [(local-context? state)     (withf state
                                         $local-vars  (curry stack:update-front (curry var-table:declare-var-with-box name box)))]
      [(class-def-context? state) (update* (curry var-table:declare-var-with-box name box)
                                           state $classes (current-type state) class:$s-fields)]
                                           
      [(top-level-context? state) (withf state
                                         $global-vars (curry var-table:declare-var-with-box name box))]
      [else                       (error "logical error, exhausted cases in declare-var")])))

;; State with this varname declared in the current scope and initialized to this values
(define declare-var-with-value
  (lambda (name value state)
    (declare-var-with-box name (box value) state)))

;; State with this varname declared in the current scope
(define declare-var
  (lambda (name state)
    (declare-var-with-value name null state)))

;; State with val assigned to this varname in the most recent scope containing such a name
; Assumptions:
; 1) var has already been initialized
; 2) get-var-box returns the box of the appropriate binding
(define assign-var
  (lambda (name val state)
    (set-box! (get-var-box name state) val)
    state))


;; Get the box that backs the in-scope var with this name
; check if initialized first
; local -> instance -> static -> global
; if dotted, restrict to instance and static
(define get-var-box
  (lambda (name state)
    (or
     (and (not (dotted? state))
          (ormap (curry var-table:var-box name)
                 (local-vars state)))
     (and (eq? 'this name) (box (this state)))
     (and (this state)
          (get-instance-field-box name (this state) (current-type state) state))
     (and (current-type state)
          (get-static-field-box name (current-type state) state))
     (and (not (dotted? state))
          (var-table:var-box name (global-vars state))))))

;; Assumes `this` is an instance of `type` (same or sub-class)
(define get-instance-field-box
  (lambda (name this class-name state)
    (ormap (curry var-table:var-box name)
           (bottom-layers (instance:fields this) (get-class-height class-name state)))))
;; Searches this class and its parents for a static field with the name. #F on miss
(define get-static-field-box
  (lambda (name class-name state)
    (if class-name
        (or (var-table:var-box name (get* state $classes class-name class:$s-fields))
            (get-static-field-box name
                                  (class:parent (get-class class-name state))
                                  state))
        #F)))

;; get the current value of this var
; assumption: var is declared, get-var-box works correctly
(define get-var-value
  (lambda (name state)
    (unbox (get-var-box name state))))

;; Is a variable with this name in scope?
; assumption: get-var-box returns #F on miss
(define var-declared?
  (lambda (name state)
    (not (false? (get-var-box name state)))))

;; Is a variable with this name already declared in the current scope?
; used by interpreter to detect duplicate declarations
(define var-already-declared?
  (lambda (name state)
    (cond
      [(class-def-context? state) (class:has-field? name (get* state $classes (current-type state)))]
      [(local-context? state)     (var-table:var-declared? name (stack:peek (local-vars state)))]
      [(top-level-context? state) (var-table:var-declared? name (global-vars state))])))


;; Is the in-scope variable with this name and initialized?
; assumptions:
; 1) uninitialized variables store `boxed null`
; 2) this is only called on variables known to be declared
; 3) get-var-value returns the appropriate variable
(define var-initialized?
  (lambda (name state)
    (not (null? (get-var-value name state)))))





;;;;;;;;;;;;;;;; FUNCTIONS


;; Is a function with this signature in the current scope
; used by interpreter to decide whether a declaration collides with an existing function
(define fun-already-declared?
  (lambda (name arg-list state)
    (fun-table:has? name arg-list (get-current-fun-table name state))))
; current table where declarations would go
(define get-current-fun-table
  (lambda (f-name state)
    (cond
      [(class-def-context? state) => (lambda (class-name)
                                       (get* state $classes class-name (if (eq? f-name class-name)
                                                                           class:$constructors
                                                                           class:$methods)))]
      [(local-context? state)         (stack:peek (local-funs state))]
      [(top-level-context? state)     (global-funs state)]
      [else                           (error "exhausted cases in fun lookup. logical error?")])))

;; Is a function with this signature in scope (reachable)?
; Assumptions:
; 1) get-function returns #F on miss
; 2) get-function performs the lookup in the correct order
(define has-fun?
  (lambda (name arg-list state)
    (not (false? (get-function name arg-list state)))))

;; sweep through local, global, classes
; primarily used by interpreter to display suggestions when reporting errors
; for calling an undefined function
; WIP
(define all-funs-with-sig
  (lambda (name state)
    (foldl append
           '()
           (map (curry fun-table:get-all name)
                (append (global-funs state)
                        (if (current-type state)
                            (get* state $classes (current-type state) class:$methods)
                            '())
                        (local-funs state))))))
;; Get closure for the first function with this signature
; traverses scopes in this order:
; local (new->old) -> instance (sub->super) -> static (sub->super) -> global
(define get-function
  (lambda (name arg-list state)
    (or (ormap (curry fun-table:get name arg-list) (local-funs state))
        (get-inst/abst-method name arg-list (this state) (current-type state) state)
        (get-static-method name arg-list (current-type state) state)
        (fun-table:get name arg-list (global-funs state)))))

;; list of all the methods declared in this class
(define get-class-methods
  (lambda (class-name state)
    (class:methods (get-class class-name state))))
; w/ polymorphism, returns the appropriate instance or abstract method for this.
; #F on miss or if the given `compile-time type` doesn't have access to such a method
(define get-inst/abst-method
  (lambda (name arg-list this class-name state)
    (and this
         class-name
         (class-has-i-a-method? name arg-list class-name state)
         (get-i-a-method-rec name
                             arg-list
                             (if (super state)
                                 (get-parent-name class-name state)
                                 (instance:class this))
                             state))))
; does this type declare or inherit an instance or abstract method w/ this signature
(define class-has-i-a-method?
  (lambda (name arg-list class-name state)
    (if class-name
        (or (class-declares-inst-or-abst-method? class-name name arg-list state)
            (class-has-i-a-method? name arg-list (get-parent-name class-name state) state))
        #F)))
;; class's declared or inherited instance method w/ this signature
; used when verifying that an abstract method will not override a concrete method
; #f on miss
(define class-get-inst-method
  (lambda (class-name fun-name params state)
    (if class-name
        (or (fun-table:get fun-name params (get-class-i-methods class-name state))
            (class-get-inst-method (get-parent-name class-name state) fun-name params state))
        #f)))
; this particular class's inst or abst method w/ given signature. #F on miss
(define get-class-i-a-method
  (lambda (class-name fun-name params state)
    (fun-table:get fun-name
                   params
                   (get-class-i-a-methods class-name state))))
;; whether this class declares an instance method with this signature
; used when verifying that abstract methods were overridden
; assumes get returns #f on miss
(define class-declares-inst-or-abst-method? get-class-i-a-method)
; returns the (most recent) instance method w/ this signature in this class's ancestry
; assumes such a method exists.
(define get-i-a-method-rec
  (lambda (name arg-list class-name state)
    (or (get-class-i-a-method class-name
                              name
                              arg-list
                              state)
        (get-i-a-method-rec name
                            arg-list
                            (get-parent-name class-name state)
                            state))))
;; list of all the instance or abstract methods declared in this class
; assumes valid class
(define get-class-i-a-methods
  (lambda (class-name state)
    (filter (join-or function:abstract? function:instance?)
            (get-class-methods class-name state))))
;; list of all the instance methods declared in this class
; assumes valid class
(define get-class-i-methods
  (lambda (class-name state)
    (filter function:instance? (get-class-methods class-name state))))
;; list of all the static methods declared in this class
; assumes valid class
(define get-class-s-methods
  (lambda (class-name state)
    (filter function:static? (get-class-methods class-name state))))

;; searches this class and its superclasses for a static method with this signature
; assumes type is not #F
; internal
; #F on miss
(define get-static-method
  (lambda (name arg-list class-name state)
    (if class-name
        (or (fun-table:get name
                           arg-list
                           (get-class-s-methods class-name state))
            (get-static-method name
                               arg-list
                               (get-parent-name class-name state)
                               state))
        #F)))

;; returns a list of the abstract methods of the parent of this class
; assume valid class-name
; empty list if no parent
(define get-parents-abstract-methods
  (lambda (class-name state)
    (if (has-parent? class-name state)
        (filter function:abstract?
                (class:methods (get-parent class-name state)))
        null)))


;; Get the init function for this class
; #F on miss
(define get-init
  (lambda (class-name state)
    (get* state $classes class-name class:$init)))

;; Get closure for the constructor with this signature
; #F on miss
; assumes class exists and constructors have class as name
(define get-constructor
  (lambda (class-name arg-list state)
    (fun-table:get class-name arg-list (get* state $classes class-name class:$constructors))))

;; State with this fun declared in the current scope
; only called for top-level or nested functions
(define declare-fun
  (lambda (name params body state)
    (cond
      ; special case of local - inherit `type` of enclosing function
      [(fun-call-context? state)  =>   (lambda (fc-fun)
                                         (declare-fun-local name
                                                            params
                                                            body
                                                            (function:scope fc-fun)
                                                            (function:class fc-fun)
                                                            state))]
      ; general case of local - if not in fun-call, assume free
      ; ex: in top-level block statement. free but not global
      [(local-context? state)          (declare-fun-local name
                                                          params
                                                          body
                                                          function:scope:free
                                                          #F
                                                          state)]
      ; not in funcall or class body, not in block etc
      [(top-level-context? state)      (declare-fun-global name
                                                           params
                                                           body
                                                           state)]
      ; todo, replace with separate declare-method function
      [else                            (error "exhausted cases in declare-fun -> logical error")])))

; global case of declare-fun
(define declare-fun-global
  (lambda (name params body state)
    (withf state
           $global-funs  (curry fun-table:declare-fun
                                name
                                params
                                body
                                (make-scoper state #F #F)
                                function:scope:free
                                #F))))

; local case of declare-fun
(define declare-fun-local
  (lambda (name params body scope class state)
    (withf state
           $local-funs  (curry stack:update-front
                               (curry fun-table:declare-fun
                                      name
                                      params
                                      body
                                      (make-scoper state class (eq? function:scope:instance scope))
                                      scope
                                      class)))))






;;;;;;;;;;;; CLASS

(define has-class?
  (lambda (class-name state)
    (map:in*? state $classes class-name)))

; given name of class, returns closure. #F if absent
(define get-class
  (lambda (class-name state)
    (map:get* state $classes class-name)))

(define current-type-has-parent?
  (lambda (state)
    (has-parent? (current-type state) state)))
;; returns name and closure of the parent of this class
; assumes valid class-name. assumes get* returns false on miss. #F if no parent
(define has-parent?
  (lambda (class-name state)
    (not (false? (get-parent class-name state)))))

(define get-parent-name
  (lambda (class-name state)
    (map:get* state $classes class-name class:$parent)))

(define get-parent
  (lambda (class-name state)
    (get-class (get-parent-name class-name state) state)))

(define get-class-height
  (lambda (class-name state)
    (if (has-parent? class-name state)
        (+ 1 (get-class-height (get-parent-name class-name state) state))
        1)))

;; declares an empty class
(define declare-class
  (lambda (class-name parent-name state)
    (withf state
           $classes  (curry map:put class-name (class:of #:name class-name #:parent parent-name))
           $context-stack  (curry stack:push (context:of-class-def class-name))
           $current-type  (lambda (v) class-name))))

; declare an instance field for a class (just name, no initializer)
(define declare-instance-field
  (lambda (name class-name state)
    (update* (curry cons name)
             state $classes class-name class:$i-field-names)))

;; called during class body
; handles static | instance | abstract
(define declare-method
  (lambda (name params body scope class state)
    (update* (curry fun-table:declare-fun
                    name
                    params
                    body
                    (make-scoper state class (eq? function:scope:instance scope))
                    scope
                    class)
             state $classes class class:$methods)))
(define method-already-declared?
  (lambda (class-name fun-name arg-list state)
    (fun-table:get fun-name arg-list (get-class-methods class-name state))))
; assumes it is called in class-def context
(define declare-inst-field
  (lambda (field-name state)
    (update* (curry cons field-name)
             state $classes (current-type state) class:$i-field-names)))
;; add an init function to a class
; assumption: called exactly once during class-body
(define declare-init
  (lambda (body class state)
    (put* (function:of #:name 'init
                       #:params '()
                       #:body body
                       #:scoper (make-scoper state class #T)
                       #:scope function:scope:init
                       #:class class)
          state $classes class class:$init)))

;; add a constructor to a class
; assumption: interpreter checks that this constructor has a unique signature
(define declare-constructor
  (lambda (params body class state)
    (update* (curry fun-table:declare-fun
                    class
                    params
                    body
                    (make-scoper state class #T)
                    function:scope:constructor
                    class)
             state $classes class class:$constructors)))

;; signal that the earlier class declaration has ended
; assumes the current context is a class-decl context
(define end-class-decl
  (lambda (class-name state)
    (pop-context state)))

;; returns a 0-initialized instance of the given class
; all fields set to 0, before any initializers
; assumes valid class-name
(define get-zero-init-instance
  (lambda (class-name state)
    (instance:of #:class class-name
                 #:fields (build-instance-fields class-name state))))
; 0 is used as the default val for 
(define default-val 0)

(define build-instance-fields
  (lambda (class-name state)
    (if class-name
        (cons (foldl (lambda (k vt)
                       (var-table:declare-var-with-box k (box default-val) vt))
                     new-var-table
                     (class:i-field-names (get-class class-name state)))
              (build-instance-fields (get-parent-name class-name state) state))
        null)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;; Unit Tests
;; more thorough coverage is currently handled by the functional-tests (v1-v3)
(module+ test
  (require rackunit)
  (define s1 (push-new-layer (push-top-level-context new-state)))
  (define s2 (declare-var 'a s1))
  (check-true (var-declared? 'a s2))
  (check-false (var-initialized? 'a s2))
  
  (define s3 (assign-var 'a 3 s2))
  (check-true (var-initialized? 'a s3))
  (check-eq? (get-var-value 'a s3) 3)
  
  (define s4 (push-new-layer s3))
  (check-false (var-already-declared? 'a s4))
  (check-eq? (get-var-value 'a s4) 3)
  (define s5 (declare-var-with-value 'a 7 s4))
  (check-eq? (get-var-value 'a s5) 7)
  (define s6 (pop-layer s5))
  (check-eq? (get-var-value 'a s6) 3)

  (define s7 (declare-var-with-box 'd (box 5)
                                   (declare-var 'c
                                                (declare-var-with-value 'b #T
                                                                        (push-new-layer s6)))))
  (check-eq? (get-var-value 'd s7) 5)
  (check-false (var-initialized? 'c s7))
  (check-eq? (get-var-value 'd s7) 5)
  
  (define s8 (assign-var 'd 10 (push-new-layer s7)))
  (check-eq? (get-var-value 'd s8) 10)

  ;; local fun lookup
  (let* ([s1      (push-top-level-context new-state)]
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