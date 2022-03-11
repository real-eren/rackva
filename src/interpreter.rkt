#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSDS 343 Interpreter Part 1
;; 2022 Spring
;; Group 1
;; Duc Huy Nguyen, Eren Kahriman, Loc Nguyen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "state.rkt"
         "util/map.rkt"
         "simpleParser.rkt")

(provide interpret
         interpret-str)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; High-level functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; takes a file name, interprets the program,
;; and returns the result
(define interpret
  (lambda (file-name)
    (extract-result (Mstate-stmt-list (parser file-name) new-state))))

;; takes a string representing a program, intreprets it
;; returns the result
(define interpret-str
  (lambda (str)
    (extract-result (Mstate-stmt-list (parser-str str) new-state))))

;; takes a state and returns its return value
;; modifies booleans
(define extract-result
  (lambda (state)
    (cond
      [(not (state-return? state))                     (error "program ended without reaching a return statement")]
      [(eq? #t (state-get-return-value state))         'true]
      [(eq? #f (state-get-return-value state))         'false]
      [(number? (state-get-return-value state))        (state-get-return-value state)]
      [else                                            (error "returned a value, but unsupported type: "
                                                              (state-get-return-value state))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Mstate functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;; STATEMENT LIST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; executes a list of statements with a given initial state
;; and returns the resulting state
(define Mstate-stmt-list
  (lambda (statement-list state)
    (cond
      [(state-return? state)              state]
      [(null? statement-list)             state]
      [else                               (Mstate-stmt-list (cdr statement-list)
                                                            (Mstate-statement (car statement-list)
                                                                              state))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; STATEMENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes a statement
;; returns the state resulting from evaluating it
(define Mstate-statement
  (lambda (statement state)
    (cond
      [(state-return? state)                state] ; exit early on return
      [(null? statement)                    (error "called Mstate on null statement")]
      [(is-construct? statement)            ((get-Mstate statement) statement state)]
      [else                                 (error "unrecognized stmt:" statement)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BLOCK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns a list of the statements in a block statement
(define block-stmt-list cdr)

;; takes a block statement
;; returns the state resulting from evaluating it
(define Mstate-block
  (lambda (statement state)
    (state-pop-frame (Mstate-stmt-list (block-stmt-list statement) (state-push-new-frame state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXPRESSION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes an expression and a state
;; returns the state after evaluating the expression
;;  in the context of the given initial state
(define Mstate-expr
  (lambda (expr state)
    (cond
      [(null? expr)                          (error "called Mstate on null expr")]
      ; base case: 1 | x
      [(not (nested? expr))                  state]
      ; else nested
      ; x = expr
      [(is-assign? expr)                     (Mstate-assign expr state)]
      [(has-op? expr)                        (Mstate-op expr state)]
      [else                                  (error "unrecognized expr" expr)])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WHILE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; takes a while statement
; returns an expression
(define while-condition second)
; returns a statement list
(define while-body third)

;; takes a statement representing a while statement
(define Mstate-while
  (lambda (statement state)
    (Mstate-while-impl (while-condition statement)
                       (while-body statement)
                       state)))

(define Mstate-while-impl
  (lambda (condition body state)
    (if (Mbool condition state)
        (Mstate-while-impl condition
                           body
                           (Mstate-statement body
                                             (Mstate-expr condition state)))
        (Mstate-expr condition state))))


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
  (lambda (statement state)
    (Mstate-if-impl (if-condition statement)
                    (if-then-body statement)
                    (if-else-body statement)
                    state)))

(define Mstate-if-impl
  (lambda (condition then-body maybe-else-body state)
    (cond
      [(Mbool condition state)            (Mstate-statement then-body
                                                            (Mstate-expr condition state))]
      [(null? maybe-else-body)            (Mstate-expr condition state)]
      [else                               (Mstate-statement (get maybe-else-body)
                                                            (Mstate-expr condition state))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RETURN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; takes a statement representing a return statement
; extracts the expression portion
(define return-expr-part second)

; takes a statement representing a return statement
; returns the resulting state,
;  which will have also set a special var in state corresponding to the return value
(define Mstate-return
  (lambda (statement state)
    (state-set-return-value (Mvalue (return-expr-part statement) state)
                            (Mstate-expr (return-expr-part statement) state))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DECLARE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes a statement representing a declaration statement
; var x; | var x = expr
(define decl-var second)
(define decl-maybe-expr cddr)

;; takes a declaration statement
;; returns the resulting state
(define Mstate-declare
  (lambda (statement state)
    (Mstate-decl-impl (decl-var statement)
                      (decl-maybe-expr statement)
                      state)))

;; declares the variable,
;; error if already declared
;; initializes if expr is provided
(define Mstate-decl-impl
  (lambda (var-name maybe-expr state)
    (cond
      [(state-var-declared-top-frame? var-name state)   (error (string-append "attempted to re-declare "
                                                                              (symbol->string var-name)
                                                                              "."))]
      [(null? maybe-expr)                               (state-declare-var var-name state)]
      [else                                             (state-assign-var var-name
                                                                          (Mvalue (get maybe-expr) state)
                                                                          (Mstate-expr (get maybe-expr)
                                                                                       (state-declare-var var-name state)))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ASSIGN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes a list representing a declaration statement
; x = expr
(define assign-var second)
(define assign-expr third)

;; assigns the value resulting from evaluating expr
;;  onto the state resulting from evaluating expr
(define Mstate-assign
  (lambda (expr state)
    (Mstate-assign-impl (assign-var expr)
                        (assign-expr expr)
                        state)))

(define Mstate-assign-impl
  (lambda (var-name val-expr state)
    (if (state-var-declared? var-name state)
        (state-assign-var var-name
                          (Mvalue val-expr state)
                          (Mstate-expr val-expr state))
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
;; also handles short-circuiting
(define Mstate-op
  (lambda (expr state)
    (cond
      [(is-||? expr)          (Mstate-|| (op-param-list expr) state)]
      [(is-&&? expr)          (Mstate-&& (op-param-list expr) state)]
      [else                   (state-after-expr-list
                               (sort-list-to-associativity-of-op (op-op-symbol expr)
                                                                 (op-param-list expr))
                               state)])))

;; takes a list of two expressions and a state
;; returns state resulting from performing short-circuit or
;; if the first expression evals to true, doesn't eval second
(define Mstate-||
  (lambda (exprs state)
    (if (Mbool (first exprs) state)
        (Mstate-expr (first exprs) state)
        (Mstate-expr (second exprs) (Mstate-expr (first exprs) state)))))

;; takes a list of two expressions and a state
;; returns state resulting from performing short-circuit and
;; if the first expression evals to false, doesn't eval second
(define Mstate-&&
  (lambda (exprs state)
    (if (Mbool (first exprs) state)
        (Mstate-expr (second exprs) (Mstate-expr (first exprs) state))
        (Mstate-expr (first exprs) state))))


;; takes an op-symbol and a list of params
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

;; Takes a nested boolean expression (with two args)
(define bool-left-op second)
(define bool-right-op third)

;; takes an expression and evaluates it
;; error if not bool
(define Mbool
  (lambda (expr state)
    (cond
      [(not (nested? expr))      (assert-bool (Mvalue-base expr state))]
      [(is-||? expr)             (or (Mbool (bool-left-op expr)
                                            state)
                                     (Mbool (bool-right-op expr)
                                            (Mstate-expr (bool-left-op expr) state)))]
      [(is-&&? expr)             (and (Mbool (bool-left-op expr)
                                             state)
                                      (Mbool (bool-right-op expr)
                                             (Mstate-expr (bool-left-op expr) state)))]
      [else                      (assert-bool (Mvalue-op expr state))])))


; error if not bool, else allows through
(define assert-bool
  (lambda (val)
    (if (boolean? val)
        val
        (error "" val " is not a bool"))))


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
      [(is-assign? expr)                   (Mvalue (assign-expr expr) state)]
      [(is-boolean? expr)                  (Mbool expr state)]
      [(has-op? expr)                      (Mvalue-op expr state)]
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


;; retrieves value of a var from state
;; throws appropriate errors if undeclared or uninitialized
(define read-var
  (lambda (var-symbol state)
    (cond
      [(not (state-var-declared? var-symbol state))       (error (string-append "referenced "
                                                                                (symbol->string var-symbol)
                                                                                " before declaring it."))]
      [(not (state-var-initialized? var-symbol state))    (error (string-append "accessed "
                                                                                (symbol->string var-symbol)
                                                                                " before initializing it."))]
      [else                                               (state-var-value var-symbol state)])))


;; takes a nested expression containing an op
;; and evaluates it
(define Mvalue-op
  (lambda (expr state)
    (op-apply (op-of-symbol (op-op-symbol expr))
              (map-expr-list-to-value-list
               (sort-list-to-associativity-of-op (op-op-symbol expr)
                                                 (op-param-list expr))
               state))))


;; takes a list of exprs and maps them to values,
;; propagating the state changes (so that they evaluate correctly)
(define map-expr-list-to-value-list
  (lambda (expr-list state)
    (if (null? expr-list)
        expr-list
        (cons (Mvalue (car expr-list) state)
              (map-expr-list-to-value-list (cdr expr-list)
                                           (Mstate-expr (car expr-list) state))))))


;; takes an op and a val-list * already in order of associativity
;; returns the value of the op applied to the list of values
(define op-apply
  (lambda (op val-list)
    (cond
      [(eq? 1 (length val-list))                  (op (first val-list))]
      [(eq? 2 (length val-list))                  (op (first val-list) (second val-list))]
      [else                                       (error op val-list)])))
; todo:
; check types
; check # params expected by op (more-so for functions)
; generalize to N params with `values` and `call-with-values`
    



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; takes a nested expr (list), returns what should be an action symbol
;; use is-___ functions to determine whether the
;;  returned symbol actually represents a particular action
(define action car)

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
(define is-assign? (checker-of '=))
(define is-declare? (checker-of 'var))
(define is-return? (checker-of 'return))
(define is-if? (checker-of 'if))
(define is-while? (checker-of 'while))
(define is-break? (checker-of 'break))
(define is-continue? (checker-of 'continue))
(define is-block? (checker-of 'begin))

;; keys are checker functions that take a statement and return a bool
;; values are the corresponding constructs
(define constructs-table (map-from-interlaced-entry-list
                          (list is-return?  Mstate-return
                                is-while?   Mstate-while
                                is-if?      Mstate-if
                                is-declare? Mstate-declare
                                is-assign?  Mstate-assign
                                is-block?   Mstate-block)
                          (map-empty-custom (lambda (key checker) (checker key)))))

;; returns whether the statement is a recognized construct
(define is-construct?
  (lambda (statement)
    (map-contains? statement constructs-table)))

;; returns the Mstate function that goes with this statement,
;; assuming it is a valid construct
(define get-Mstate
  (lambda (statement)
    (map-get statement constructs-table)))



;; returns whether a nested expression is of the boolean variety
(define is-boolean?
  (lambda (nested-expr)
    (map-contains? (action nested-expr) boolean-op-table)))

(define is-&&? (checker-of '&&))
(define is-||? (checker-of '||))

;; Takes a nested expression and returns whether it contains a recognized operation
(define has-op?
  (lambda (expr)
    (or (map-contains? (action expr) arithmetic-op-table)
        (map-contains? (action expr) boolean-op-table))))


;; associative lists from op-symbols to functions
; the alternative is a (hard-coded) cond
; which is a less flexible design
; consider these to be constants

(define boolean-op-table
  (map-from-interlaced-entry-list
   (list '&& (lambda (a b) (and a b))
         '|| (lambda (a b) (or a b))
         '!  not
         '== eq?
         '!= (lambda (a b) (not (eq? a b)))
         '<  <
         '>  >
         '<= <=
         '>= >=)
   map-empty))

(define arithmetic-op-table
  (map-from-interlaced-entry-list
   (list '+  +
         '-  -
         '/  quotient
         '*  *
         '%  modulo)
   map-empty))

;; assuming the atom is an op-symbol, returns the associated function
(define op-of-symbol
  (lambda (op-symbol)
    (if (map-contains? op-symbol arithmetic-op-table)
        (map-get op-symbol arithmetic-op-table)
        (map-get op-symbol boolean-op-table))))



