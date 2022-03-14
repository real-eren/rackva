#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSDS 343 Interpreter Part 1
;; 2022 Spring
;; Group 1
;; Duc Huy Nguyen, Eren Kahriman, Loc Nguyen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "conts.rkt"
         "state.rkt"
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
    (Mstate-stmt-list (parser file-name)
                      new-state
                      (conts-of
                       #:return (lambda (v) (prep-val-for-output v))))))

;; takes a string representing a program, intreprets it
;; returns the result
(define interpret-str
  (lambda (str)
    (Mstate-stmt-list (parser-str str)
                      new-state
                      (conts-of
                       #:return (lambda (v) (prep-val-for-output v))))))

;; takes a value and modifies it for output
(define prep-val-for-output
  (lambda (value)
    (cond
      [(eq? #t value)         'true]
      [(eq? #f value)         'false]
      [(number? value)        value]
      [else                   (error "returned an unsupported type: " value)])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Mstate functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;; STATEMENT LIST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; executes a list of statements with a given initial state
;; and returns the resulting state
(define Mstate-stmt-list
  (lambda (statement-list state conts)
    (if (null? statement-list)
        ((next conts) state)
        (Mstate-statement (car statement-list)
                          state
                          (conts-of conts
                                    #:next (lambda (s)
                                             (Mstate-stmt-list (cdr statement-list) 
                                                               s
                                                               conts)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; STATEMENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes a statement
;; returns the state resulting from evaluating it
(define Mstate-statement
  (lambda (statement state conts)
    (cond
      [(null? statement)                    (error "called Mstate on null statement")]
      [(is-construct? statement)            ((get-Mstate statement) statement state conts)]
      [else                                 (error "unrecognized stmt:" statement)])))

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
                      (state-push-new-frame state)
                      (conts-of conts
                                #:next (lambda (s)
                                         ((next conts) (state-pop-frame s)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXPRESSION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes an expression and a state
;; returns the state after evaluating the expression
;;  in the context of the given initial state
(define Mstate-expr
  (lambda (expr state evaluate)
    (cond
      [(null? expr)                          (error "called Mstate on null expr")]
      ; base case: 1 | x
      [(not (nested? expr))                  (evaluate state)]
      ; else nested
      ; x = expr
      [(is-assign? expr)                     (Mstate-assign expr state evaluate)]
      [(has-op? expr)                        (Mstate-op expr state evaluate)]
      [else                                  (error "unrecognized expr" expr)])))


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
           conts
           (lambda (b s1)
             (if b
                 (Mstate-statement body 
                                   s1
                                   (conts-of conts
                                             #:next (lambda (s2)
                                                      (Mstate-while-impl condition
                                                                         body
                                                                         s2
                                                                         conts))))
                 ((next conts) s1))))))


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
           conts
           (lambda (b s)
             (cond
               [b                        (Mstate-statement then-body s conts)]
               [(null? maybe-else-body)  ((next conts) s)]
               [else                     (Mstate-statement (get maybe-else-body) s conts)])))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RETURN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; takes a statement representing a return statement
; extracts the expression portion
(define return-expr-part second)

; takes a statement representing a return statement
; returns the resulting state
(define Mstate-return
  (lambda (statement state conts)
    (Mvalue (return-expr-part statement)
            state
            conts
            (lambda (v s)
              ((return conts) v)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DECLARE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes a statement representing a declaration statement
; var x; | var x = expr
(define decl-var second)
(define decl-maybe-expr cddr)

;; takes a declaration statement
;; returns the resulting state
(define Mstate-declare
  (lambda (statement state conts)
    (Mstate-decl-impl (decl-var statement)
                      (decl-maybe-expr statement)
                      state
                      conts)))

;; declares the variable,
;; error if already declared
;; initializes if expr is provided
(define Mstate-decl-impl
  (lambda (var-name maybe-expr state conts)
    (cond
      [(state-var-declared-top-frame? var-name state)   (error (string-append "attempted to re-declare "
                                                                              (symbol->string var-name)))]
      [(null? maybe-expr)                               ((next conts) (state-declare-var var-name state))]
      [else                                             (Mvalue (get maybe-expr)
                                                                state
                                                                conts
                                                                (lambda (v s)
                                                                  ((next conts) (state-assign-var var-name 
                                                                                                  v 
                                                                                                  (state-declare-var var-name s)))))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ASSIGN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes a list representing a declaration statement
; x = expr
(define assign-var second)
(define assign-expr third)

;; assigns the value resulting from evaluating expr
;;  onto the state resulting from evaluating expr
(define Mstate-assign
  (lambda (expr state conts)
    (Mstate-assign-impl (assign-var expr)
                        (assign-expr expr)
                        state
                        conts)))

(define Mstate-assign-impl
  (lambda (var-name val-expr state conts)
    (if (state-var-declared? var-name state)
        (Mvalue val-expr state conts (lambda (v s)
                                       ((next conts) (state-assign-var var-name v s))))
        ; else assigning to undeclared var
        (error (string-append "tried to assign to "
                              (symbol->string var-name)
                              " before declaring it.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TRY CATCH FINALLY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define try-body second) ; may return null
(define try-catch third) ; may return null
(define try-finally fourth) ; may return null

(define Mstate-try
  (lambda (statement state conts)
    (Mstate-block-impl (try-body statement) state conts))) ;TODO: add catch and finally handling




;; Deprecated (see Mvalue)
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
  (lambda (expr state evaluate)
    (cond
      [(is-||? expr)          (Mstate-|| (op-param-list expr) state evaluate)]
      [(is-&&? expr)          (Mstate-&& (op-param-list expr) state evaluate)]
      [else                   (state-after-expr-list
                               (sort-list-to-associativity-of-op (op-op-symbol expr)
                                                                 (op-param-list expr))
                               state
                               evaluate)])))

;; takes a list of two expressions and a state
;; returns state resulting from performing short-circuit or
;; if the first expression evaluates to true, doesn't evaluate second
(define Mstate-||
  (lambda (exprs state evaluate)
    (Mbool (first exprs) state (lambda (b s) 
                                 (if b
                                     (evaluate s)
                                     (Mbool (second exprs) s (lambda (b s) (evaluate s))))))))

;; takes a list of two expressions and a state
;; returns state resulting from performing short-circuit and
;; if the first expression evaluates to false, doesn't evaluate second
(define Mstate-&&
  (lambda (exprs state evaluate)
    (Mbool (first exprs) state (lambda (b s) 
                                 (if b
                                     (Mbool (second exprs) s (lambda (b s) (evaluate s)))
                                     (evaluate s))))))


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
  (lambda (expr-list state evaluate)
    (if (null? expr-list)
        (evaluate state)
        (Mstate-expr (car expr-list) state (lambda (s) 
                                             (state-after-expr-list (cdr expr-list) s evaluate))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Mboolean functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Takes a nested boolean expression (with two args)
(define bool-left-op second)
(define bool-right-op third)

;; takes an expression and evaluate it
;; error if not bool
(define Mbool
  (lambda (expr state conts evaluate)
    (cond
      [(not (nested? expr))      (Mvalue-base expr state conts (lambda (v s)
                                                                 (evaluate (assert-bool v) s)))]
      [(is-||? expr)             (Mbool (bool-left-op expr)
                                        state
                                        conts
                                        (lambda (b1 s1)
                                          (Mbool (bool-right-op expr)
                                                 s1
                                                 conts
                                                 (lambda (b2 s2)
                                                   (evaluate (or b1 b2) s2)))))]
      [(is-&&? expr)             (Mbool (bool-left-op expr)
                                        state
                                        conts
                                        (lambda (b1 s1)
                                          (Mbool (bool-right-op expr)
                                                 s1
                                                 conts
                                                 (lambda (b2 s2)
                                                   (evaluate (and b1 b2) s2)))))]
      [else                      (Mvalue-op expr
                                            state
                                            conts
                                            (lambda (b s)
                                              (evaluate (assert-bool b) s)))])))


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
  (lambda (expr state conts evaluate)
    (cond
      [(null? expr)                        (error "called Mvalue on a null expression")]
      [(not (nested? expr))                (Mvalue-base expr state conts evaluate)]
      ; else nested expr
      [(is-assign? expr)                   (Mvalue (assign-expr expr) 
                                                   state
                                                   conts
                                                   (lambda (v s) 
                                                     (evaluate v (state-assign-var (assign-var expr) v s))))]
      [(is-boolean? expr)                  (Mbool expr state conts evaluate)]
      [(has-op? expr)                      (Mvalue-op expr state conts evaluate)]
      [else                                (error "unreachable in Mvalue")])))


;; returns the value of the token given the state
;; token = 1 | 'x | 'true 
(define Mvalue-base
  (lambda (token state conts evaluate)
    (cond
      [(number? token)            (evaluate token state)]
      [(eq? 'true token)          (evaluate #t state)]
      [(eq? 'false token)         (evaluate #f state)]
      [else                       (evaluate (read-var token state) state)])))


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
  (lambda (expr state conts evaluate)
    (map-expr-list-to-value-list
     (sort-list-to-associativity-of-op (op-op-symbol expr)
                                       (op-param-list expr))
     state
     conts
     (lambda (v1 s) 
       (op-apply (op-of-symbol (op-op-symbol expr)) 
                 v1
                 (lambda (v2)
                   (evaluate v2 s)))))))


;; takes a list of exprs and maps them to values,
;; propagating the state changes (so that they evaluate correctly)
(define map-expr-list-to-value-list
  (lambda (expr-list state conts evaluate)
    (if (null? expr-list)
        (evaluate expr-list state)
        (Mvalue (car expr-list)
                state
                conts
                (lambda (v1 s1)
                  (map-expr-list-to-value-list (cdr expr-list)
                                               s1
                                               conts
                                               (lambda (v2 s2) (evaluate (cons v1 v2) s2))))))))


;; takes an op and a val-list * already in order of associativity
;; returns the value of the op applied to the list of values
(define op-apply
  (lambda (op val-list evaluate)
    (cond
      [(eq? 1 (length val-list))                  (evaluate (op (first val-list)))]
      [(eq? 2 (length val-list))                  (evaluate (op (first val-list) (second val-list)))]
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



