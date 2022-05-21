#lang racket

(require "conts.rkt"
         "user-errors.rkt"
         "state/function.rkt"
         "state/instance.rkt"
         "state/state.rkt"
         "util/map.rkt"
         "util/predicates.rkt"
         "classParser.rkt")

(provide interpret
         interpret-parse-tree-v3
         interpret-parse-tree-v2
         interpret-parse-tree-v1

         default-return
         default-throw
         default-user-exn)

;;;; Interpreter
;; This module interprets programs parsed by the parsers
;; `simpleParser.rkt`, `functionParser.rkt`, `classParser.rkt`
;; Tests for this module are in the `v{n}-tests` and `v{n}-error-tests`
;; files, where {n} corresponds to the version of the interpreter being tested


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; High-level functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; takes a file name and a string class name, interprets the program,
;; and returns the result
(define interpret
  (lambda (file-name class-name)
    (interpret-parse-tree-v3 (parser file-name)
                             class-name
                             default-return
                             default-throw
                             (default-user-exn))))

;; interprets parse-trees produced by classParser.rkt
(define interpret-parse-tree-v3
  (lambda (parse-tree entry-point return throw user-exn)
    (Mstate-stmt-list parse-tree
                      new-state
                      (conts-of
                       #:next (lambda (s)
                                (Mstate-main s
                                             return
                                             throw
                                             user-exn
                                             #:class (string->symbol entry-point)))
                       #:throw throw
                       #:user-exn user-exn))))

;;interprets parse-trees produced by functionParser.rkt
(define interpret-parse-tree-v2
  (lambda (parse-tree return throw user-exn)
    (Mstate-stmt-list parse-tree
                      new-state
                      (conts-of ; only next and throw are actually reachable
                       #:next (lambda (s) (Mstate-main s return throw user-exn))
                       #:throw throw
                       #:user-exn user-exn))))

;; legacy, for testing
;; interprets parse-trees produced by simpleParser.rkt
(define interpret-parse-tree-v1
  (lambda (simple-parse-tree return throw user-exn)
    (Mstate-stmt-list simple-parse-tree
                      new-state
                      (conts-of
                       #:return return
                       #:next (lambda (s) (user-exn (ue:did-not-return) s))
                       #:throw throw
                       #:break (default-break user-exn)
                       #:continue (default-continue user-exn)
                       #:user-exn user-exn))))

;; takes a value and modifies it for output
(define prep-val-for-output
  (lambda (value)
    (cond
      [(eq? #t value)         'true]
      [(eq? #f value)         'false]
      [(number? value)        value]
      [(is-instance? value)   value]
      [else                   (error "returned an unsupported type: " value)])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Mstate functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TOP-LEVEL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Find and execute the main function with the initial state
; should be run after executing all other top-level statements
(define Mstate-main
  (lambda (state return throw user-exn #:class [class #f])
    (if (and class (not (state:has-class? class state)))
        (user-exn (ue:not-a-class class) state)
        (Mvalue-fun '(funcall main)
                    (if class
                        (state:set-static-scope class state)
                        state)
                    (conts-of
                     #:throw throw
                     #:return return
                     #:user-exn user-exn)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; STATEMENT LIST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; executes a list of statements with a given initial state
;; and returns the resulting state
; optionally takes a filter function (on statement) and throws an error if an
; encountered statement does not pass the filter
(define Mstate-stmt-list
  (lambda (stmt-list state conts)
    (if (null? stmt-list)
        ((next conts) state)
        (Mstate-statement (first stmt-list)
                          state
                          (conts-of conts
                                    #:next (lambda (s)
                                             (Mstate-stmt-list (rest stmt-list) 
                                                               (state:copy-context-stack #:src state #:dest s)
                                                               conts)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; STATEMENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes a statement
;; returns the state resulting from evaluating it
(define Mstate-statement
  (lambda (statement state conts)
    ((get-Mstate statement) statement (state:push-context statement state) conts)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CLASS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define class-name second)
(define maybe-extend third)
(define parent-or-null
  (lambda (maybe-extend)
    (if (null? maybe-extend)
        null
        (second maybe-extend))))
(define class-body fourth)

;; Takes a class declaration statement and adds it to the state
(define Mstate-class-decl
  (lambda (statement state conts)
    (Mstate-class-decl-impl (class-name statement)
                            (parent-or-null (maybe-extend statement))
                            (class-body statement)
                            state
                            conts)))

(define Mstate-class-decl-impl
  (lambda (class-name parent body state conts)
    (cond
      [(state:has-class? class-name state)    ((user-exn conts) (ue:duplicate-class class-name) state)]
      [(eq? class-name parent)                ((user-exn conts) (ue:class-extend-self class-name) state)]
      [(or (null? parent)
           (state:has-class? parent state))   (eval-class-body class-name
                                                               parent
                                                               body
                                                               (state:declare-class class-name parent state)
                                                               (next conts)
                                                               (throw conts)
                                                               (user-exn conts))]
      [else                                   ((user-exn conts) (ue:not-a-class parent) state)])))



;; Run through the body of a class and apply the declarations to state
; Assumptions:
; 1) runs after `state:declare-class` and before `state:end-class-decl`
(define eval-class-body
  (lambda (class-name parent body state next throw user-exn)
    (chain-TR state
              next
              ; declare methods and verify that parent's abstract methods are overridden
              (λ (s nxt) (methods (filter is-method? body) class-name parent s nxt user-exn))
              (λ (s nxt) (verify-abstracts-overridden class-name s nxt user-exn))
              ; declare instance fields (name only), and initializers (init)
              (λ (s nxt) (inst-fields class-name (filter is-inst-field-decl? body) s nxt user-exn))
              ; declare user-defined constructors
              (λ (s nxt) (constructors class-name (filter is-ctor-decl? body) s nxt user-exn))
              ; add default constructor if no user-defined constructors present
              (lambda (s nxt)
                (if (no-constructors? body)
                    (declare-constructor default-constructor-stmt class-name s nxt user-exn)
                    (nxt s)))
              ; declare static fields w/ values
              (λ (s nxt) (static-fields (filter is-static-field-decl? body) class-name s nxt throw user-exn)))))


;; joins a sequence of tail-recursive functions via next continuations
;; each function must take two args: state and next(s)
(define chain-TR
  (lambda (state last-next f . fs)
    (if (null? fs)
        (f state last-next)
        (f state (lambda (s)
                   (apply chain-TR s last-next fs))))))

;;;;;;;; METHOD DECLARATIONS

;; returns the function:scope if stmt is a method declaration, and #f if not a method declaration
; Assumptions:
; 1) stmt is a list with >1 element
; 2) this is only called on statements within a class body
(define method-type-table
  (map:of
   'abstract-function  function:scope:abstract
   'static-function    function:scope:static
   'function           function:scope:instance))
; Assumptions:
; 1) input stmts were already validated
(define method-type
  (lambda (stmt)
    (map:get (first stmt) method-type-table)))
; 2) map:get return #F on miss
(define is-method? method-type)

;; evaluate a list of method declarations in a class body
; after all declared, check that parent's abstract methods were overridden
(define methods
  (lambda (method-decl-list class-name parent state next user-exn)
    (if (null? method-decl-list)
        (next state)
        (declare-method (first method-decl-list)
                        class-name
                        state
                        (lambda (s)
                          (methods (rest method-decl-list)
                                   class-name
                                   parent
                                   s
                                   next
                                   user-exn))
                        user-exn))))
; these assume a well-formed method declaration statement
(define method-name second)
(define method-params third)
(define method-body-or-null
  (lambda (stmt)
    (if (null? (cdddr stmt))
        null
        (fourth stmt))))

(define declare-method
  (lambda (stmt class-name state next user-exn)
    (declare-method-impl (method-name stmt)
                         (method-params stmt)
                         (method-body-or-null stmt)
                         (method-type stmt)
                         class-name
                         state
                         next
                         user-exn)))

(define declare-method-impl
  (lambda (m-name m-params m-body m-type class-name state next user-exn)
    (cond
      [(super-or-this? m-name)                           (user-exn (ue:keyword-as-identifier m-name 'method) state)]
      [(state:method-already-declared? class-name
                                       m-name
                                       m-params
                                       state)            (user-exn (ue:duplicate-method (function:formatted-signature m-name m-params)
                                                                                        class-name)
                                                                   state)]
      [(and (eq? function:scope:abstract m-type)
            (state:class-get-inst-method class-name
                                         m-name
                                         m-params
                                         state))   =>    (lambda (fun)
                                                           (user-exn (ue:override-c-w/-abstr (function->string fun)) state))]
      [else                                              (next (state:declare-method m-name
                                                                                     m-params
                                                                                     m-body
                                                                                     m-type
                                                                                     class-name
                                                                                     state))])))
;; Assumes all instance&abstract methods of this class have been declared
; verify that all of parent's abstract methods are redeclared in this class
(define verify-abstracts-overridden
  (lambda (class-name state next user-exn)
    (verify-abstracts-impl class-name
                           (state:get-parents-abstract-methods class-name state)
                           state
                           next
                           user-exn)))

(define verify-abstracts-impl
  (lambda (class-name abstract-funs state next user-exn)
    (cond
      [(null? abstract-funs)                 (next state)]
      [(fun-overridden? (first abstract-funs)
                        class-name
                        state)               (verify-abstracts-impl class-name
                                                                    (rest abstract-funs)
                                                                    state
                                                                    next
                                                                    user-exn)]
      [else                                  (user-exn (ue:unoverridden-abstract class-name
                                                                                 (function->string (first abstract-funs)))
                                                       state)])))
; whether a class declares an override for a method
; to be called on abstract methods of parent
(define fun-overridden?
  (lambda (fun class-name state)
    (state:class-declares-inst-or-abst-method? class-name
                                               (function:name fun)
                                               (function:params fun)
                                               state)))

;;;;;;;; STATIC FIELD DECLARATIONS
(define static-fields
  (lambda (body class-name state next throw user-exn)
    (if (null? body) 
        (next state)
        (declare-static-field (first body)
                              class-name
                              state
                              (lambda (s)
                                (static-fields (rest body) class-name s next throw user-exn))
                              throw
                              user-exn))))

(define declare-static-field
  (lambda (stmt class-name state next throw user-exn)
    (let* ([name        (decl-var stmt)]
           [maybe-expr  (decl-maybe-expr stmt)]
           [expr        (if (null? maybe-expr) 0 (get maybe-expr))])
      (cond
        [(super-or-this? name)                      (user-exn (ue:keyword-as-identifier name 'field) state)]
        [(state:field-already-declared? name
                                        class-name
                                        state)      (user-exn (ue:duplicate-field name) state)]
        [else                                       (Mvalue expr
                                                            state
                                                            (conts-of #:throw throw
                                                                      #:user-exn user-exn
                                                                      #:return (lambda (v s)
                                                                                 (next (state:declare-static-field name
                                                                                                                   v
                                                                                                                   class-name
                                                                                                                   s)))))]))))


;;;;;;;; INSTANCE FIELD DECLARATIONS

(define inst-fields
  (lambda (class-name instance-field-decls state next user-exn)
    (declare-inst-fields class-name
                         instance-field-decls
                         state
                         (lambda (s)
                           (declare-init class-name
                                         instance-field-decls
                                         s
                                         next))
                         user-exn)))

(define declare-inst-fields
  (lambda (class-name i-field-decls state next user-exn)
    (if (null? i-field-decls)
        (next state)
        (declare-inst-field class-name
                            (decl-var (first i-field-decls))
                            state
                            (lambda (s)
                              (declare-inst-fields class-name
                                                   (rest i-field-decls)
                                                   s
                                                   next
                                                   user-exn))
                            user-exn))))

(define declare-inst-field
  (lambda (class-name var-name state next user-exn)
    (cond
      [(super-or-this? var-name)                      (user-exn (ue:keyword-as-identifier var-name 'field) state)]
      [(state:field-already-declared? var-name
                                      class-name
                                      state)          (user-exn (ue:duplicate-field var-name) state)]
      [else                                           (next (state:declare-instance-field var-name
                                                                                          class-name
                                                                                          state))])))
;; add the fields with initializers to the classes init method
; map to an assignment
(define declare-init
  (lambda (class-name i-field-decls state next)
    (next (state:declare-init (map decl->assign
                                   (filter (lambda (decl)
                                             (not (null? (decl-maybe-expr decl))))
                                           i-field-decls))
                              class-name
                              state))))
(define decl->assign
  (lambda (decl-stmt)
    (list '= (decl-var decl-stmt) (get (decl-maybe-expr decl-stmt)))))

;;;;;;;; CONSTRUCTOR DECLARATIONS
(define ctor-params second)
(define ctor-body third)

(define default-constructor-stmt '(constructor () ()))
(define no-constructors?
  (lambda (body)
    (false? (findf is-ctor-decl? body))))

(define constructors
  (lambda (class-name body state next user-exn)
    (if (null? body)
        (next state)
        (declare-constructor (first body)
                             class-name 
                             state
                             (lambda (s)
                               (constructors class-name (rest body) s next user-exn))
                             user-exn))))

(define declare-constructor
  (lambda (stmt class-name state next user-exn)
    (if (state:ctor-already-declared? class-name (ctor-params stmt) state)
        (user-exn (ue:duplicate-constructor (function:formatted-signature class-name (ctor-params stmt))) state)
        (next (state:declare-constructor (ctor-params stmt)
                                         (ctor-body stmt)
                                         class-name
                                         state)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BLOCK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns a list of the statements in a block statement
(define block-stmt-list cdr)

;; takes a block statement
;; returns the state resulting from evaluating it
(define Mstate-block
  (lambda (statement state conts)
    (Mstate-block-impl (block-stmt-list statement) state conts)))
;; takes a statement list
(define Mstate-block-impl
  (lambda (stmt-list state conts)
    (Mstate-stmt-list stmt-list
                      (state:push-new-layer state)
                      (w/preproc conts #:map-state state:pop-layer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WHILE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; takes a while statement
; returns an expression
(define while-condition second)
; returns a statement list
(define while-body third)

;; takes a statement representing a while statement
(define Mstate-while
  (lambda (statement state conts)
    (Mstate-while-impl (while-condition statement)
                       (while-body statement)
                       state
                       conts)))

(define Mstate-while-impl
  (lambda (condition body state conts)
    (Mbool condition
           state
           (conts-of conts
                     #:return (lambda (b s1)
                                (if b
                                    (Mstate-statement body 
                                                      s1
                                                      (conts-of conts
                                                                #:next (lambda (s2)
                                                                         (Mstate-while-impl condition
                                                                                            body
                                                                                            s2
                                                                                            conts))
                                                                #:break (next conts)
                                                                #:continue (lambda (s3)
                                                                             (Mstate-while-impl condition
                                                                                                body
                                                                                                s3
                                                                                                conts))))
                                    ((next conts) s1)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; IF ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; takes an if statement
; returns an expression
(define if-condition second)
; returns a statement list
(define if-then-body third)
; returns a maybe-statement
(define if-else-body cdddr)

;; takes a statement representing an if statement
;; returns the resulting state
(define Mstate-if
  (lambda (statement state conts)
    (Mstate-if-impl (if-condition statement)
                    (if-then-body statement)
                    (if-else-body statement)
                    state
                    conts)))

(define Mstate-if-impl
  (lambda (condition then-body maybe-else-body state conts)
    (Mbool condition
           state
           (conts-of conts
                     #:return (lambda (b s)
                                (cond
                                  [b                        (Mstate-statement then-body s conts)]
                                  [(null? maybe-else-body)  ((next conts) s)]
                                  [else                     (Mstate-statement (get maybe-else-body) s conts)]))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RETURN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; takes a statement representing a return statement
; extracts the expression portion
(define return-expr-part second)

; takes a statement representing a return statement
; invokes the return continuation
(define Mstate-return
  (lambda (statement state conts)
    (Mvalue (return-expr-part statement)
            state
            conts)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; THROW ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; takes a statement representing a throw statement
; extracts the expression portion
(define throw-expr-part second)

; takes a statement representing a throw statement
; invokes the throw continuation
(define Mstate-throw
  (lambda (statement state conts)
    (Mvalue (throw-expr-part statement)
            state
            (conts-of conts
                      #:return (throw conts)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;   FUNCTION DECLARATION   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; takes a statement representing a function declaration statement
; declare the function in the state

(define decl-fun-name second)
(define decl-fun-params third)
(define decl-fun-body fourth)

; called for top-level and nested fun declarations
(define Mstate-fun-decl
  (lambda (statement state conts)
    (Mstate-fun-decl-impl (decl-fun-name statement) 
                          (decl-fun-params statement) 
                          (decl-fun-body statement) 
                          state
                          conts)))

(define Mstate-fun-decl-impl
  (lambda (fun-name fun-params fun-body state conts)
    (if (state:fun-already-declared? fun-name fun-params state)
        ((user-exn conts) (ue:duplicate-function (function:formatted-signature fun-name fun-params)) state)
        ((next conts) (state:declare-fun fun-name 
                                         fun-params
                                         fun-body
                                         state)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;   FUNCTION INVOCATION   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; takes a statement representing a function statement
; invokes the function

(define Mstate-fun-call
  (lambda (statement state conts)
    (Mshared-fun statement
                 state
                 (conts-of conts
                           #:next       (lambda (s) ((next conts) state))
                           #:return     (lambda (v s) ((next conts) state))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BREAK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; takes a statement representing a break statement
; invokes the break continuation
(define Mstate-break
  (lambda (statement state conts)
    ((break conts) state)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CONTINUE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; takes a statement representing a continue statement
; invokes the continue continuation
(define Mstate-continue
  (lambda (statement state conts)
    ((continue conts) state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DECLARE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes a statement representing a declaration statement
; var x; | var x = expr
(define decl-var second)
(define decl-maybe-expr cddr)

;; takes a declaration statement
;; returns the resulting state
(define Mstate-var-decl
  (lambda (statement state conts)
    (Mstate-var-decl-impl (decl-var statement)
                          (decl-maybe-expr statement)
                          state
                          conts)))

;; declares the variable,
;; error if already declared
;; initializes if expr is provided
(define Mstate-var-decl-impl
  (lambda (var-name maybe-expr state conts)
    (cond
      [(super-or-this? var-name)                      ((user-exn conts) (ue:keyword-as-identifier var-name 'variable) state)]
      [(state:var-already-declared? var-name state)   ((user-exn conts) (ue:duplicate-variable var-name) state)]
      [(null? maybe-expr)                             ((next conts) (state:declare-var var-name state))]
      [else                                           (Mvalue (get maybe-expr)
                                                              state
                                                              (conts-of conts
                                                                        #:return (lambda (v s)
                                                                                   ((next conts) (state:declare-var-with-value var-name 
                                                                                                                               v
                                                                                                                               s)))))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ASSIGN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; assigns the value resulting from evaluating expr
;;  onto the state resulting from evaluating expr
(define Mstate-assign
  (lambda (expr state conts)
    (Mvalue-assign expr state (conts-of conts
                                        #:return (lambda (v s)
                                                   ((next conts) s))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TRY CATCH FINALLY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define try-body second) ; may return null
(define try-catch third) ; may return null
;; takes the catch portion
(define catch-var (compose1 first second))
(define catch-body third)
(define try-finally fourth) ; may return null
;; takes the finally portion
(define finally-body second)

(define Mstate-try
  (lambda (statement state conts)
    (Mstate-try-impl (try-body statement)
                     (try-catch statement)
                     (try-finally statement)
                     state
                     conts)))

(define Mstate-try-impl
  (lambda (try-body catch-block finally-block state conts)
    
    (define (try-next default-cont)
      (if (null? finally-block)
          default-cont
          (lambda (s)
            (Mstate-block-impl (finally-body finally-block)
                               s
                               (conts-of conts
                                         #:next default-cont)))))
    
    (define (finally-next next)
      (lambda (s)
        (Mstate-block-impl (finally-body finally-block)
                           s
                           (conts-of conts #:next next))))
    
    (define try-throw
      (lambda (e s)
        (define old-cs-state (state:copy-context-stack #:src state #:dest s))
        (cond
          [(null? catch-block)   (Mstate-block-impl (finally-body finally-block)
                                                    old-cs-state
                                                    (conts-of conts
                                                              #:next (lambda (s2)
                                                                       ((throw conts) e (state:copy-context-stack #:src s #:dest s2)))))]
          [(null? finally-block) (Mstate-block-impl (catch-body catch-block)
                                                    (state:declare-var-with-value (catch-var catch-block)
                                                                                  e
                                                                                  old-cs-state)
                                                    conts)]
          [else                  (Mstate-block-impl (catch-body catch-block)
                                                    (state:declare-var-with-value (catch-var catch-block)
                                                                                  e
                                                                                  old-cs-state)
                                                    (conts-of conts ; during catch, before finally
                                                              #:next (finally-next (next conts))
                                                              #:break (finally-next (break conts))
                                                              #:continue (finally-next (continue conts))
                                                              #:throw (lambda (e2 s2)
                                                                        (Mstate-block-impl (finally-body finally-block)
                                                                                           s2
                                                                                           (conts-of conts
                                                                                                     #:next (lambda (s3)
                                                                                                              ((throw conts) e2 s3)))))
                                                              #:return (lambda (v s2)
                                                                         (Mstate-block-impl (finally-body finally-block)
                                                                                            s2
                                                                                            (conts-of conts
                                                                                                      #:next (lambda (s3)
                                                                                                               ((return conts) v s3)))))))])))
    (Mstate-block-impl try-body
                       state
                       (conts-of conts
                                 #:next (try-next (next conts))
                                 #:break (try-next (break conts))
                                 #:continue (try-next (continue conts))
                                 #:return (if (null? finally-block)
                                              (return conts)
                                              (lambda (v s)
                                                (Mstate-block-impl (finally-body finally-block)
                                                                   s
                                                                   (conts-of conts
                                                                             #:next (lambda (s2)
                                                                                      ((return conts) v s2))))))
                                 #:throw try-throw))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Mboolean functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Takes a nested boolean expression (with two args)
(define bool-left-op second)
(define bool-right-op third)

;; takes an expression and evaluates it
;; error if not bool
(define Mbool
  (lambda (expr state conts)
    (Mbool-impl expr state (w/preproc conts #:map-value assert-bool)))) ; todo, refactor assert-bool to use user-exn
; Like Mvalue, but produces bool else error, and handles short-circuiting
(define Mbool-impl
  (lambda (expr state conts)
    (cond
      [(or (number? expr)
           (eq? 'true expr)
           (eq? 'false expr))    (Mvalue-literal expr state conts)]
      [(name? expr)              (Mvalue-var expr state conts)]
      [(is-||? expr)             (Mbool (bool-left-op expr)
                                        state
                                        (conts-of conts
                                                  #:return (lambda (b1 s1)
                                                             (if b1
                                                                 ((return conts) b1 s1)
                                                                 (Mbool (bool-right-op expr)
                                                                        s1
                                                                        conts)))))]
      [(is-&&? expr)             (Mbool (bool-left-op expr)
                                        state
                                        (conts-of conts
                                                  #:return (lambda (b1 s1)
                                                             (if (not b1)
                                                                 ((return conts) b1 s1)
                                                                 (Mbool (bool-right-op expr)
                                                                        s1
                                                                        conts)))))]
      [(is-assign? expr)         (Mvalue expr state conts)]
      [(is-nested-boolean-expr?
        expr)                    (Mvalue-op expr state conts)]
      [(is-fun-call? expr)       (Mvalue-fun expr state conts)]
      [else                      (error "not considered to be a boolean type expr: " expr)])))


; error if not bool, else allows through
(define assert-bool
  (lambda (val)
    (if (boolean? val)
        val
        (error "value is not a bool:" val))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Mvalue functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns the value of an expression, in the context of the given state
(define Mvalue
  (lambda (expr state conts)
    (cond
      [(or (number? expr)
           (eq? 'true expr)
           (eq? 'false expr))               (Mvalue-literal expr state conts)]
      [(name? expr)                         (Mvalue-var expr state conts)]
      [(is-new? expr)                       (Mvalue-new expr state conts)]
      [(is-fun-call? expr)                  (Mvalue-fun expr state conts)]
      [(is-assign? expr)                    (Mvalue-assign expr state conts)]
      [(is-nested-boolean-expr? expr)       (Mbool expr state conts)]
      [(has-op? expr)                       (Mvalue-op expr state conts)]
      [else                                 (error "unreachable in Mvalue")])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VARS & FIELDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Mbox-var
  (lambda (expr state conts)
    (Mname expr state (conts-of conts
                                #:return (lambda (n s)
                                           (read-var-box n
                                                         s
                                                         (λ (v s) ((return conts) v state))
                                                         (user-exn conts)))))))

(define Mvalue-var
  (lambda (expr state conts)
    (Mbox-var expr state (w/preproc conts
                                    #:map-value unbox))))

;; Given a simple name, retrieves box/value of a var from state
;; throws appropriate errors if undeclared or uninitialized
(define read-var-box
  (lambda (name state return user-exn)
    (cond
      [(eq? 'super name)                             (user-exn (ue:keyword-as-identifier 'super 'variable) state)]
      [(not (state:var-declared? name state))        (user-exn (ue:reference-undeclared-var name) state)]
      [(not (state:var-initialized? name state))     (user-exn (ue:access-uninitialized-var name) state)]
      [else                                          (return (state:get-var-box name state) state)])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LITERALS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns the value of the token given the state
;; token = 1 |'false | 'true 
(define Mvalue-literal
  (lambda (token state conts)
    ((return conts) (Mvalue-literal-impl token state) state)))

(define Mvalue-literal-impl
  (lambda (token state)
    (cond
      [(number? token)            token]
      [(eq? 'true token)          #t]
      [(eq? 'false token)         #f]
      [else                       (error "not a bool/int literal" token)])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Mvalue-fun
  (lambda (expr state conts)
    (Mshared-fun expr
                 state
                 (conts-of conts ; todo: refactor funcall arg, move to context stack
                           #:next (lambda (s) ((user-exn conts) (ue:no-return-value-fun (funcall->string expr)) state))
                           #:return (lambda (v s) ((return conts) v state))))))

;; given a valid funcall AST node, produces a user-readable
; string that should resemble the source code.
(define funcall->string
  (lambda (expr)
    (function:formatted-signature (fun-name expr) (fun-inputs expr))))

(define fun-name second)
(define fun-inputs cddr)

;; common logic for Mstate-fun and Mvalue-fun
(define Mshared-fun
  (lambda (expr state conts)
    (Mshared-fun-impl (fun-name expr)
                      (fun-inputs expr)
                      state
                      conts)))

(define Mshared-fun-impl
  (lambda (name arg-list state conts)
    (define (on-hit n s)
      (Mvalue-fun-impl (state:get-function n arg-list s)
                       arg-list
                       state
                       s
                       ; passing state in next, throw and return,
                       ; because the s returned from fun call was scoped
                       ; aka the continuations that leave the fun-body (return to call-site)
                       ; boxes handle the side-effects from the function body
                       ; however s2 has the context-stack during fun body, needed for error messages
                       ; continue, break and throw need s2's context stack
                       (conts-of
                        #:next     (lambda (s2) ((next conts) state))
                        #:throw    (lambda (e s2)
                                     ((throw conts) e (state:copy-context-stack #:src s2 #:dest state)))
                        #:return   (lambda (v s2) ((return conts) v state))
                        #:break    (default-break (user-exn conts))
                        #:continue (default-continue (user-exn conts))
                        #:user-exn (user-exn conts))))
    (Mname name
           state
           (conts-of conts
                     #:return (lambda (n s)
                                (cond
                                  [(super-or-this? n)             ((user-exn conts) (ue:ctor-chain-outside-ctor n) state)]
                                  [(state:has-fun? n arg-list s)  (on-hit n s)]
                                  [else                           ((user-exn conts) (ue:function-not-in-scope name
                                                                                                              (length arg-list)
                                                                                                              (get-similar-fun-suggestions n state))
                                                                                    state)]))))))
(define get-similar-fun-suggestions
  (lambda (name state)
    (let ([similar-funs  (state:all-funs-with-name name state)])
      (if (null? similar-funs)
          ""
          (string-join (map function->string similar-funs)
                       "\n"
                       #:before-first "\nDid you mean one of the following?\n----------\n"
                       #:after-last "\n----------\n")))))

(define Mvalue-fun-impl
  (lambda (fun-closure fun-inputs eval-state state conts)
    (if (function:abstract? fun-closure)
        ((user-exn conts) (ue:invoke-abstract-method (function->string fun-closure)) state)
        (get-environment fun-closure
                         fun-inputs
                         eval-state
                         (state:set-fun-call-context fun-closure state)
                         (conts-of conts
                                   #:next (lambda (s)
                                            (Mstate-stmt-list (function:body fun-closure)
                                                              s
                                                              conts)))))))


;; Evaluates parameters and produces the state in which the function body will be evaluated
(define get-environment
  (lambda (fun inputs eval-state out-state conts)
    (boxed-arg-list-cps (function:params fun)
                        inputs
                        eval-state
                        conts
                        (lambda (p l)
                          ((next conts) (bind-boxed-params p
                                                           l
                                                           (state:push-new-layer ((function:scoper fun) out-state)))))
                        (lambda (p s)
                          ((user-exn conts) (ue:non-var-in-ref-param (function->string fun) p) s)))))

;; Takes in the inputs and params and the current state, return the mapping of params and values
;; The evaluation passing the list of boxes of input, the params without the & and the new state 
(define boxed-arg-list-cps
  (lambda (formal-params actual-params eval-state conts evaluation param-ref-error)
    (cond
      [(and (null? actual-params)
            (null? formal-params))      (evaluation '() '())]
      ;; by value
      [(not (eq? (first formal-params)
                 '&))                   (Mvalue (first actual-params)
                                                eval-state
                                                (conts-of conts
                                                          #:return (lambda (v1 s1)
                                                                     (boxed-arg-list-cps (rest formal-params)
                                                                                         (rest actual-params)
                                                                                         s1
                                                                                         conts
                                                                                         (lambda (ps bs)
                                                                                           (evaluation (cons (first formal-params) ps)
                                                                                                       (cons (box v1) bs)))
                                                                                         param-ref-error))))]
      ;; by reference
      [(eq? 'this (first actual-params)) ((user-exn conts) (ue:this-as-ref-param) eval-state)]
      [(name? (first actual-params))     (boxed-arg-list-cps (cddr formal-params)
                                                             (cdr actual-params)
                                                             eval-state
                                                             conts
                                                             (lambda (ps bs)
                                                               (Mbox-var (first actual-params)
                                                                         eval-state
                                                                         (conts-of conts
                                                                                   #:return (lambda (b s)
                                                                                              (evaluation (cons (second formal-params) ps)
                                                                                                          (cons b bs))))))
                                                             param-ref-error)]
      [else                             (param-ref-error (second formal-params)
                                                         eval-state)])))

;; Binds the names of the formal-params to boxes representing the actual parameters
; before calling this, push a layer to state
(define bind-boxed-params
  (lambda (formal-params box-list state)
    (foldl state:declare-var-with-box
           state
           formal-params
           box-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ASSIGN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes a list representing a declaration statement
; x = expr
(define assign-var second)
(define assign-expr third)

(define Mvalue-assign
  (lambda (expr state conts)
    (Mvalue (assign-expr expr)
            state
            (conts-of conts
                      #:return (lambda (v s)
                                 (Mname (assign-var expr)
                                        s
                                        (conts-of conts
                                                  #:return (lambda (n s2)
                                                             (cond
                                                               [(or (eq? 'super n)
                                                                    (eq? 'this n))           ((user-exn conts) (ue:assigning-to-this/super n) state)]
                                                               [(state:var-declared? n s2)   ((return conts) v (state:restore-scope #:dest (state:assign-var n v s2)
                                                                                                                                    #:src state))]
                                                               [else                         ((user-exn conts) (ue:reference-undeclared-var n) state)])))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   NEW   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define new-class second)
(define new-params cddr)

;; construct an object
(define Mvalue-new 
  (lambda (statement state conts)
    (Mvalue-new-impl (new-class statement)
                     (new-params statement)
                     state
                     conts)))

(define Mvalue-new-impl
  (lambda (class-name arg-list state conts)
    (if (state:has-class? class-name state)
        ; changes to instance during constructors are handled by boxes
        (let ([inst  (state:get-zero-init-instance class-name state)])
          (construct-instance class-name
                              arg-list
                              '()
                              state
                              (state:set-instance-scope inst state)
                              (conts-of
                               #:next (lambda (s) ((return conts) inst state))
                               #:return (lambda (v s) ((return conts) inst state))
                               #:throw (lambda (e s) ((throw conts) e state))
                               #:break (default-break (user-exn conts))
                               #:continue (default-continue (user-exn conts))
                               #:user-exn (user-exn conts))))
        ((user-exn conts) (ue:not-a-class class-name) state))))

;; Search for constructor and run. Error if absent
(define construct-instance
  (lambda (class-name arg-list ctor-chain arg-eval-state run-state conts)
    (if (state:get-constructor class-name arg-list arg-eval-state)
        (ctor-rec (state:get-constructor class-name arg-list arg-eval-state)
                  arg-list
                  class-name
                  ctor-chain
                  arg-eval-state
                  run-state
                  conts)
        ((user-exn conts) (ue:ctor-DNE class-name 
                                       (string-join (map (curry format "~a") arg-list)
                                                    ", "
                                                    #:before-first "("
                                                    #:after-last ")"))
                          arg-eval-state))))

;; Evaluate constructor closure. Recurses into other constructors if necessary
(define ctor-rec
  (lambda (ctor args class-name ctor-chain arg-eval-state run-state conts)
    (if (member ctor ctor-chain)
        ((user-exn conts) (ue:cyclic-ctor-chaining) run-state)
        (get-environment ctor
                         args
                         arg-eval-state
                         (state:set-fun-call-context ctor run-state)
                         (conts-of conts
                                   #:next (lambda (s)
                                            (ctor-rec-impl (function:body ctor)
                                                           class-name
                                                           (state:get-parent-name class-name arg-eval-state)
                                                           (cons ctor ctor-chain)
                                                           s
                                                           conts)))))))

(define ctor-rec-impl
  (lambda (body class-name parent ctor-chain state conts)
    ; recursively do parent constructors + inits
    (define next
      (lambda (s)
        (Mstate-stmt-list (if (or (start-super? body)
                                  (start-this? body))
                              (cdr body)
                              body)
                          state
                          conts)))
    (ctor-pre-body body
                   class-name
                   parent
                   ctor-chain
                   state
                   (conts-of conts
                             ; then do body of current constructor
                             #:next next
                             #:return (λ (v s) (next s))))))
;; handles pre-body: super+init | this
; init function was generated, and will not contain a return statement
(define ctor-pre-body
  (lambda (body class-name parent ctor-chain state conts)
    (define super-next
      (lambda (s)
        (Mvalue-fun-impl (state:get-init class-name state)
                         '()
                         state
                         state
                         conts)))
    (cond
      ; this -> run that ctor
      [(start-this? body)               (construct-instance class-name
                                                            (fun-inputs (first body))
                                                            ctor-chain
                                                            state
                                                            state
                                                            conts)]
      ; super and no parent -> error
      [(and (start-super? body)
            (not parent))               ((user-exn conts) (ue:super-w/out-parent class-name) state)]
      ; no parent -> init
      [(not parent)                     (Mvalue-fun-impl (state:get-init class-name state)
                                                         '()
                                                         state
                                                         state
                                                         conts)]
      ; else parent -> super (default if not specified), then init
      [else                             (construct-instance parent
                                                            (if (start-super? body)
                                                                (fun-inputs (first body))
                                                                '())
                                                            '() ; empty chain because going up class heirarchy, can't revisit ctors of this class
                                                            state
                                                            state
                                                            (conts-of conts
                                                                      #:next super-next
                                                                      #:return (lambda (v s) (super-next s))))])))

; assumes every statement in a function body is nested and has at least 2 elems
;; is the first statement of a ctor body this(...) or super(...)
(define start-this?
  (lambda (body)
    (and (list? body)
         (nor (null? body)
              (null? (first body))
              (null? (cdr (first body))))
         (is-this-ctor? (first body)))))

(define start-super?
  (lambda (body)
    (and (list? body)
         (nor (null? body)
              (null? (first body))
              (null? (cdr (first body))))
         (is-super-ctor? (first body)))))

(define is-this-ctor?
  (lambda (expr)
    (eq? (second expr) 'this)))
(define is-super-ctor?
  (lambda (expr)
    (eq? (second expr) 'super)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXPRESSIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes an expression representing one with an operator
; ex: -(expr) | !(bool-expr) | -x | x+y | z/2 | etc
(define op-op first) ; returns symbol of op
(define op-param-list cdr)

;; takes a nested expression containing an op
;; and evaluates it
(define Mvalue-op
  (lambda (expr state conts)
    (eval-expr-list (op-param-list expr)
                    state
                    (conts-of conts
                              #:return (lambda (val-list s)
                                         ((return conts) (op-apply (op-of-symbol (op-op expr)) val-list) s))))))


;; takes a list of exprs and maps them to values,
;; propagating the state changes (so that they evaluate correctly)
(define eval-expr-list
  (lambda (expr-list state conts)
    (if (null? expr-list)
        ((return conts) expr-list state)
        (Mvalue (first expr-list)
                state
                (conts-of conts
                          #:return (lambda (v1 s1)
                                     (eval-expr-list (rest expr-list)
                                                     s1
                                                     (conts-of conts
                                                               #:return (lambda (v2 s2)
                                                                          ((return conts) (cons v1 v2) s2))))))))))


;; takes an op and a val-list * already in order of associativity
;; returns the value of the op applied to the list of values
(define op-apply
  (lambda (op val-list)
    (cond
      [(eq? 1 (length val-list))       (op (first val-list))]
      [(eq? 2 (length val-list))       (op (first val-list) (second val-list))]
      [else                            (error op val-list)])))

;; assuming the atom is an op-symbol, returns the associated function
(define op-of-symbol
  (lambda (op-symbol)
    (or (map:get op-symbol arithmetic-op-table)
        (map:get op-symbol boolean-op-table))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Mname ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dot-LHS second)
(define dot-RHS third)

(define Mname
  (lambda (name state conts)
    (cond
      [(symbol? name)     (Mname-simple name state conts)]
      [(is-dotted? name)  (Mname-dot (dot-LHS name)
                                     (dot-RHS name)
                                     state
                                     conts)]
      [else               ((user-exn conts) (ue:malformed-identifier name) state)])))

(define Mname-simple
  (lambda (name state conts)
    ; other checks for `this` and `super` must occur at each site that handles names,
    ; in order to give meaningful error messages and support ctor chaining
    (if (and (eq? 'this name) (not (state:instance-context? state)))
        ((user-exn conts) (ue:this/super-in-static name) state)
        ((return conts) name state))))

(define Mname-dot
  (lambda (LHS RHS state conts)
    (cond
      [(super-or-this? RHS)             ((user-exn conts) (ue:this/super-dot-RHS RHS) state)]
      ; todo: rearrange following 2 cases, group error cases and normal cases
      [(eq? 'this LHS)                  (if (state:instance-context? state)
                                            ((return conts) RHS (state:set-this-scope state))
                                            ((user-exn conts) (ue:this/super-in-static LHS) state))]
      [(eq? 'super LHS)                 (if (and (state:instance-context? state) (state:current-type-has-parent? state))
                                            ((return conts) RHS (state:set-super-scope state))
                                            ((user-exn conts) (ue:this/super-in-static LHS) state))]
      ; if not symbol, must be instance yielding expr. if symbol and reachable var, must also yield instance
      [(or (not (symbol? LHS))
           (state:var-declared? LHS
                                state)) (Mvalue LHS
                                                state
                                                (conts-of conts
                                                          #:return (lambda (v s)
                                                                     (assert-instance v
                                                                                      s
                                                                                      (λ (v s)
                                                                                        ((return conts) RHS (state:set-instance-scope v s)))
                                                                                      (user-exn conts)))))]
      [(state:has-class? LHS state)     ((return conts) RHS (state:set-static-scope LHS state))]
      [else                             ((user-exn conts) (ue:unknown-LHS-dot LHS) state)])))

(define assert-instance 
  (lambda (v s return user-exn) 
    (if (is-instance? v) 
        (return v s)
        (user-exn (ue:non-instance-dot (prep-val-for-output v)) s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; functions that may be useful in more than one context

;; takes a statement or nested expr (list), returns what should be an action symbol
(define action first)

;; takes an expression and returns whether it contains other expressions
(define nested? list?)

;; takes a maybe-value and extracts the value
;; throws error if maybe-value was `empty`
(define get car)

;; produces a function that returns whether a statement's 'action' symbol matches symbl
(define checker-of
  (lambda (symbl)
    (lambda (statement) (eq? (action statement) symbl))))

;; these take a statement and return whether it is a particular construct
(define is-return? (checker-of 'return))
(define is-assign? (checker-of '=))
(define is-var-decl? (checker-of 'var))
(define is-fun-decl? (checker-of 'function))
(define is-fun-call? (checker-of 'funcall))
(define is-class-decl? (checker-of 'class))

(define is-static-field-decl? (checker-of 'static-var))
(define is-inst-field-decl? is-var-decl?)

(define is-ctor-decl? (checker-of 'constructor))

(define is-dotted? (checker-of 'dot))
(define is-new? (checker-of 'new))


;; whether an expression can be a name.
; simple | dotted
(define name?
  (lambda (expr)
    (or (symbol? expr)
        (and (nested? expr)
             (not (null? expr))
             (is-dotted? expr)))))

;; keys are symbols representative of a construct type
;; values are the corresponding constructs (type of statement)
(define constructs-table
  (map:of
   'return   Mstate-return
   'while    Mstate-while
   'if       Mstate-if
   'class    Mstate-class-decl
   'var      Mstate-var-decl
   'function Mstate-fun-decl
   'funcall  Mstate-fun-call
   '=        Mstate-assign
   'begin    Mstate-block
   'try      Mstate-try
   'throw    Mstate-throw
   'break    Mstate-break
   'continue Mstate-continue))

;; returns whether the statement is a recognized construct
(define is-construct?
  (lambda (statement)
    (map:contains? (action statement) constructs-table)))

;; returns the Mstate function that goes with this statement,
;; assuming it is a valid construct
(define get-Mstate
  (lambda (statement)
    (map:get (action statement) constructs-table)))


;; returns whether a nested expression is of the boolean variety
(define is-nested-boolean-expr?
  (lambda (nested-expr)
    (map:contains? (action nested-expr) boolean-op-table)))

(define is-&&? (checker-of '&&))
(define is-||? (checker-of '||))

;; Takes a nested expression and returns whether it contains a recognized operation
(define has-op?
  (lambda (expr)
    (or (map:contains? (action expr) arithmetic-op-table)
        (map:contains? (action expr) boolean-op-table))))


;; associative lists from op-symbols to functions

(define boolean-op-table
  (map:of
   '&& error ; short-circuit ops must be handled as a special case
   '|| error
   '!  not
   '== eq?
   '!= (lambda (a b) (not (eq? a b)))
   '<  <
   '>  >
   '<= <=
   '>= >=))

(define arithmetic-op-table
  (map:of
   '+  +
   '-  -
   '/  quotient
   '*  *
   '%  modulo))

; symbol n is 'this or 'super
(define super-or-this?
  (lambda (sym)
    (or (symbol=? sym 'super) (symbol=? sym 'this))))

;;;;;;;; Common Continuations

(define default-return (lambda (v s) (prep-val-for-output v)))
; interpret-parse functions should use the throw cont parameter
; false -> Mstate-throw throws a user-exn, needs conts for user-exn's context stack
(define default-throw (lambda (e s) (ue:raise-exn (ue:uncaught-exception e) s)))


(define (take-up-to lis n)
  (if (<= (length lis) n)
      lis
      (take lis n)))

(define default-user-exn
  (lambda ([n 20]) ; retain the n deepest/recent elements on the context stack
    (lambda (exn s)
      (ue:raise-exn exn (take-up-to (state:context-stack s) n)))))

(define default-break
  (lambda (user-exn)
    (lambda (s) (user-exn (ue:break-outside-loop) s))))
(define default-continue
  (lambda (user-exn)
    (lambda (s) (user-exn (ue:continue-outside-loop) s))))
