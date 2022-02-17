#lang racket
(require "state.rkt"
         "map.rkt"
         "simpleParser.rkt")

(provide interpret
         execute)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Mvalue functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; takes a file name, interprets the program,
;; and returns the result
(define interpret
  (lambda (file-name)
    (extract-result (execute (parser file-name) new-state))))

;; takes a state and returns its return value
;; modifies booleans
(define extract-result
  (lambda (state)
    (cond
      [(eq? #t (state-get-return-value state))         'true]
      [(eq? #f (state-get-return-value state))         'false]
      [(number? (state-get-return-value state))        (state-get-return-value state)]
      [else                                            (error "returned a value, but unsupported type: "
                                                              (state-get-return-value state))])))


;; executes a list of statements with a given initial state
;; and returns the resulting state
;; error if the statement list terminates w/out return
(define execute
  (lambda (statement-list state)
    (cond
      [(state-return? state)              state] 
      [(null? statement-list)             (error "program ended without reaching a return statement")]
      [else                               (execute (cdr statement-list)
                                                   (Mstate-statement (car statement-list)
                                                                     state))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; produces a function that returns whether a statement's 'action' symbol matches symbl
(define checker-of
  (lambda (symbl)
    (lambda (statement) (eq? (action statement) symbl))))

;; these take a statement and return whether it is a particular construct
(define is-assign? (checker-of '=))
(define is-declaration? (checker-of 'var))
(define is-return? (checker-of 'return))
(define is-if? (checker-of 'if))
(define is-while? (checker-of 'while))

;; returns whether the symbol matches an entry in the op-table
(define symbol-is-op?
  (lambda (symbl)
    (map-contains? symbl op-table)))

;; associative list from op-symbols to functions
; the alternative is a repetitive cond 
(define op-table
  (map-from-interlaced-entry-list
   (list '+  +
         '-  -
         '/  quotient
         '*  *
         '%  modulo
         '&& (lambda (a b) (and a b))
         '|| (lambda (a b) (or a b))
         '!  not
         '== eq?
         '!= (lambda (a b) (not (eq? a b)))
         '<  <
         '>  >
         '<= <=
         '>= >=)
   map-empty))

;; assuming the atom is an op-symbol, returns the associated function
(define op-of-symbol
  (lambda (op-symbol)
    (map-result:get-value (map-get op-symbol op-table))))


;; takes a nested expr (list), returns what should be an action symbol
;; use is-___ functions to determine whether the
;;  returned symbol actually represents a particular action
(define action car)

;; takes an expression and returns whether it contains other expressions
(define nested? list?)

;; takes a maybe-value and extracts the value
;; throws error if maybe-value was `empty`
(define unbox car)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Mstate functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; STATEMENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes a statement
;; returns the state resulting from evaluating it
(define Mstate-statement
  (lambda (statement state)
    (cond
      [(state-return? state)                state] ; exit early on return
      [(null? statement)                    (error "called Mstate on null statement")]
      [(is-return? statement)               (Mstate-return statement state)]
      [(is-while? statement)                (Mstate-while2 statement state)]
      [(is-if? statement)                   (Mstate-if2 statement state)]
      [(is-assign? statement)               (Mstate-assign2 statement state)]
      ; must be a list. first word is either a key-word or a function/op
      ; var x | var x = expr
      [(is-declaration? statement)          (Mstate-decl2 statement state)]
      [else                                 (error "unrecognized stmt:" statement)])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXPRESSION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes an expression and a state
;; returns the state after evaluating the expression
;;  in the context of the given initial state
(define Mstate-expr
  (lambda (expr state)
    (cond
      ; base case: 1 | x
      [(not (nested? expr))                    state]
      ; else nested
      ; x = expr
      [(is-assign? expr)                     (Mstate-assign2 expr state)]
      [(null? expr)                          (error "called Mstate on null expr")]
      [(symbol-is-op? (action expr))         (Mstate-op expr state)]
      [else                                  (error "unrecognized expr" expr)])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WHILE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes a statement represnting a while statement
(define while-condition second)
(define while-statement third)

;; takes a statement represnting a while statement
(define Mstate-while2
  (lambda (statement state)
    (Mstate-while (while-condition statement)
                  (while-statement statement)
                  state)))

(define Mstate-while
  (lambda (while-cond while-body state)
    (if (Mbool while-cond state)
        (Mstate-while while-cond
                      while-body
                      (Mstate-statement while-body
                                        (Mstate-expr while-cond state)))
        (Mstate-expr while-cond state))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; IF ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; takes a statement reresenting an if statement
(define if-condition second)
(define if-stmt1 third)
; returns list of size 0 or 1
(define if-maybe-stmt2 cdddr)

;; takes a statement representing an if statement
;; returns the resulting state
(define Mstate-if2
  (lambda (statement state)
    (Mstate-if (if-condition statement)
               (if-stmt1 statement)
               (if-maybe-stmt2 statement)
               state)))

(define Mstate-if
  (lambda (condition stmt1 maybe-stmt2 state)
    (cond
      [(Mbool condition state)            (Mstate-statement stmt1
                                                            (Mstate-expr condition state))]
      [(null? maybe-stmt2)                (Mstate-expr condition state)]
      [else                               (Mstate-statement (unbox maybe-stmt2)
                                                            (Mstate-expr condition state))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RETURN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; takes a statement representing a return statement
; extracts the expression portion
(define return-expression second)

; takes a statement representing a return statement
; returns the resulting state,
;  which will have also set a special var in state corresponding to the return value
(define Mstate-return
  (lambda (statement state)
    (state-set-return-value (Mvalue (return-expression statement) state)
                            (Mstate-expr (return-expression statement) state))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DECLARE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes a statement representing a declaration statement
; var x; | var x = expr
(define decl-var second)
(define decl-maybe-expr cddr)

;; takes a declaration statement
;; returns the resulting state
(define Mstate-decl2
  (lambda (statement state)
    (Mstate-decl (decl-var statement)
                 (decl-maybe-expr statement)
                 state)))

;; declares the variable,
;; error if already declared
;; initializes if expr is provided
(define Mstate-decl
  (lambda (var-name maybe-expr state)
    (cond
      [(state-var-declared? var-name state)       (error (string-append "attempted to re-declare "
                                                                        (symbol->string var-name)
                                                                        "."))]
      [(null? maybe-expr)                         (state-declare-var var-name state)]
      [else                                       (Mstate-assign var-name
                                                                 (unbox maybe-expr)
                                                                 (Mstate-decl var-name
                                                                              null
                                                                              state))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ASSIGN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes a list representing a declaration statement
; x = expr
(define assign-var second)
(define assign-expr third)

;; assigns the value resulting from evaluating expr
;;  onto the state resulting from evaluating expr
(define Mstate-assign2
  (lambda (expr state)
    (Mstate-assign (assign-var expr)
                   (assign-expr expr)
                   state)))

(define Mstate-assign
  (lambda (var-name expr state)
    (if (state-var-declared? var-name state)
        (state-assign-var var-name
                          (Mvalue expr state)
                          (Mstate-expr expr state))
        ; else assigning to undeclared var
        (error (string-append "tried to assign to "
                              (symbol->string var-name)
                              " before declaring it.")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OP EXPRESSION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes an expression representing one with an operator
; ex: -(expr) | !(bool-expr) | -x | x+y | z/2 | etc
(define op-op-symbol first)
(define op-param-list cdr)

;; takes an expression containing an operator
;; returns the state resulting from evaluating
;; the given expression in order of its operator's associativity
(define Mstate-op
  (lambda (expr state)
    (Mstate-op-helper (op-op-symbol expr)
                      (op-param-list expr)
                      state)))

;; returns the state resulting from evaluating
;; the given list of expressions in order of the given op's associativity
(define Mstate-op-helper
  (lambda (op-symbol param-list state)
    (state-after-expr-list (sort-list-to-associativity-of-op op-symbol
                                                             param-list)
                           state)))


;; takes an operator and a list of params
;; returns a list of the same params in order
;; of associativity, according to the given op
(define sort-list-to-associativity-of-op
  (lambda (op-symbol lis)
    lis)) ; all given ops are left associative


;; returns the state resulting from
;; evaluating the list of expressions
;; in the order they were given
(define state-after-expr-list
  (lambda (expr-list state)
    (if (null? expr-list)
        state
        (state-after-expr-list (cdr expr-list)
                               (Mstate-expr (car expr-list) state)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Mboolean functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes an expression and evaluates it
;; error if not bool
(define Mbool
  (lambda (expr state)
    (assert-bool (Mvalue expr state))))


; error if not bool, else allows through
(define assert-bool
  (lambda (val)
    (if (boolean? val)
        val
        (error "" val " not a bool"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Mvalue functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns the value of an expression, in the context of the given state
(define Mvalue
  (lambda (expr state)
    (cond
      [(null? expr)                        (error "called Mvalue on a null expression")]
      [(not (nested? expr))                (Mvalue-base expr state)]
      ; else nested expr
      [(symbol-is-op? (action expr))       (Mvalue-op2 expr state)]
      [(is-assign? expr)                   (Mvalue (assign-expr expr) state)]
      [else                                (error "unreachable in Mvalue")])))


;; returns the value of the token given the state
;; token = 1 | 'x | 'true 
(define Mvalue-base
  (lambda (token state)
    (cond
      [(number? token)            token]
      [(eq? 'true token)          #t]
      [(eq? 'false token)         #f]
      [else                       (read-var token state)])))


;; retrieves value of var from state
;; throws appropriate errors if undeclared or uninitialized
(define read-var
  (lambda (var-name state)
    (cond
      [(not (state-var-declared? var-name state))       (error (string-append "referenced "
                                                                              (symbol->string var-name)
                                                                              " before declaring it."))]
      [(not (state-var-initialized? var-name state))    (error (string-append "accessed "
                                                                              (symbol->string var-name)
                                                                              " before initializing it."))]
      [else                                             (state-var-value var-name state)])))


;; takes the params in order of the op's associativity
;; and returns the resulting value
(define Mvalue-op2
  (lambda (expr state)
    (Mvalue-op (op-op-symbol expr)
               (op-param-list expr)
               state)))

; ( (x == 1) or (x < 1) 
;; takes an (assumed to be valid) op-symbol and a list of its parameters
;; evals the params in order of the op's associativity
;; and returns the resulting value
(define Mvalue-op
  (lambda (op-symbol param-list state)
    (op-apply (op-of-symbol op-symbol)
                      (map-expr-list-to-value-list (sort-list-to-associativity-of-op op-symbol
                                                                                     param-list)
                                                   state))))


;; takes a list of exprs and maps them to values,
;; passing along the updated states
(define map-expr-list-to-value-list
  (lambda (expr-list state)
    (if (null? expr-list)
        expr-list
        (cons (Mvalue (car expr-list) state)
              (map-expr-list-to-value-list (cdr expr-list)
                                           (Mstate-expr (car expr-list) state))))))


;; takes an op-symbol and a val-list * already in order of associativity
;; returns the value of the op applied to the list of values
(define op-apply
  (lambda (op val-list)
    (cond
      [(eq? 1 (length val-list))                  (op (first val-list))]
      [(eq? 2 (length val-list))                  (op (first val-list) (second val-list))]
      [else                                       (error op val-list)])))
    ; todo:
    ; check types
    ; check # params expected by op
    

