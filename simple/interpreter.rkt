#lang racket
(require "state.rkt"
         "map.rkt"
         "simpleParser.rkt")

(provide interpret
         execute)


;; takes a file name and prints the result 
(define interpret
  (lambda (file-name)
    (print-result (execute (parser file-name) new-state))))

;; takes a state and prints the output
(define print-result
  (lambda (state)
    (cond
      [(not (state-return? state))                     (error "Missing return statement")] ; not needed if execute checks
      [(eq? #t (state-get-return-value state))         (print "true")]
      [(eq? #f (state-get-return-value state))         (print "false")]
      [(number? (state-get-return-value state))        (print (state-get-return-value state))]
      [else                                            (error "returned a value, but not a bool or int")])))


;; executes a list of statements with a given initial state
;; and returns the resulting value
; can modify s.t. execute returns a value
(define execute
  (lambda (statement-list state)
    ; can modify s.t. empty statement-list just returns state
    ; and move error to print-result
    (cond
      [(state-return? state)           state] 
      [(null? statement-list)          (error "program ended without reaching a return statement")]
      [else                            (execute (cdr statement-list)
                                                (Mstate-statement (car statement-list) state))])))




(define ops-and-funs-list (list '+ +
                                '- -
                                '/ quotient
                                '* *
                                '% modulo
                                '&& (lambda (a b) (and a b))
                                '|| (lambda (a b) (or a b))
                                '!  not
                                '== eq?
                                '!= (lambda (a b) (not (eq? a b)))
                                '< <
                                '> >
                                '<= <=
                                '>= >=))
;; add typing by wrapping each function
;; assoc in-types and out-types


(define int->int 0)
(define int-int->int 1)
(define int-int->bool 2)
(define bool-bool->bool 3)
(define bool->bool 4)

;; treats the first and second elems as key and value
;; returns a map with the entries
(define map-from-interlaced-entry-list
  (lambda (lis map)
    (if (null? lis)
        map
        (map-from-interlaced-entry-list (cdr (cdr lis))
                                        (map-insert (first lis) (second lis) map)))))

;; associative list from op-symbols to functions
(define op-table
  (map-from-interlaced-entry-list ops-and-funs-list map-empty))

;; returns whether the symbol matches an entry in the op-table
(define action-is-op?
  (lambda (symbl)
    (map-contains? symbl op-table)))

;; assuming the atom is an op-symbol, returns the associated function
(define op-of-symbol
  (lambda (op-symbol)
    (map-result:get-value (map-get op-symbol op-table))))


;; returns a function that returns whether an input matches a symbol
(define checker-of
  (lambda (symbol-to-match)
    (lambda (x) (eq? x symbol-to-match))))


;; these take a symbol and return whether they are of a particular class
(define action-is-assign? (checker-of '=))
(define action-is-declare? (checker-of 'var))
(define action-is-return? (checker-of 'return))
(define action-is-if? (checker-of 'if))
(define action-is-while? (checker-of 'while))



(define var-of-assign-expr second)
      

;; takes a nested expr (list), returns what should be an action symbol
;; use action-is-___ functions to determine whether the
;;  returned symbol actually represents a particular action
(define action car)


;; takes a list representing a declaration statement
; var x; | var x = expr
(define decl-var second)
(define decl-maybe-expr cddr)

;; takes a list representing a declaration statement
; x = expr
(define assign-var second)
(define assign-expr third)



; takes a list representing a return statement
(define return-expression second)

(define while-condition second)
(define while-statement third)
; takes a list reresenting an if statement
(define if-condition second)
(define if-stmt1 third)
; returns list of size 0 or 1
(define maybe-if-stmt2 cdddr)
  ; if (cond) return true
;(if (cond) (stmt))
;; takes a statement
;; returns the state resulting from evaluating it
(define Mstate-statement
  (lambda (statement state)
    (cond
      [(state-return? state)                       state] ; exit early on return
      [(null? statement)                          (error "called Mstate on null expr")]
      [(action-is-return? (action statement))     (Mstate-return (return-expression statement)
                                                                 state)]
      ; while
      [(action-is-while? (action statement))      (Mstate-while (while-condition statement)
                                                                (while-statement statement)
                                                                state)]
      ; if
      [(action-is-if? (action statement))         (Mstate-if (if-condition statement)
                                                             (if-stmt1 statement)
                                                             (maybe-if-stmt2 statement)
                                                             state)]
      ; x = expr
      [(action-is-assign? (action statement))     (Mstate-assign (assign-var statement)
                                                                 (assign-expr statement)
                                                                 state)]
      ; must be a list. first word is either a key-word or a function/op
      ; var x | var x = expr
      [(action-is-declare? (action statement))    (Mstate-decl (decl-var statement)
                                                               (decl-maybe-expr statement)
                                                               state)]
      [else                                       (error "undefined action")])))


(define Mstate-expr
  (lambda (expr state)
    (cond
      ; base case
      ; 1 | x
      [(not (list? expr))                    state]
      ; else nested
      ; x = expr
      [(action-is-assign? (action expr))     (Mstate-assign (assign-var expr)
                                                            (assign-expr expr)
                                                            state)]
      [(null? expr)                          (error "called Mstate on null expr")]
      [(action-is-op? (action expr))         (Mstate-op (op-of-symbol (action expr))
                                                        (cdr expr)
                                                        state)]
      [else                                  (error "unreachable")])))

;; Mstate for while statment
(define Mstate-while
  (lambda (while-cond while-body state)
    (if (Mbool while-cond state)
        (Mstate-while while-cond
                      while-body
                      (Mstate-statement while-body
                                        (Mstate-statement while-cond state)))
        (Mstate-statement while-cond state))))

;; Mstate for return
(define Mstate-return
  (lambda (return-expr state)
    (state-set-return-value (Mvalue return-expr state)
                            (Mstate-expr return-expr state))))

;; takes
(define Mstate-if
  (lambda (condition stmt1 maybe-stmt2 state)
    (cond
      [(Mbool condition state)      (Mstate-statement stmt1
                                                      (Mstate-statement condition state))]
      [(null? maybe-stmt2)                (Mstate-statement condition state)]
      [else                         (Mstate-statement (car maybe-stmt2)
                                                      (Mstate-statement condition state))])))

;(define Mstate-e
;; declares the variable,
;; error if already declared
;; initializes if expr is provided
(define Mstate-decl
  (lambda (var-name maybe-expr state)
    (cond
      [(state-var-declared? var-name state)       (error "attempted to re-declare "
                                                         (symbol->string var-name)
                                                         ".")]
      [(null? maybe-expr)                         (state-declare-var var-name state)]
      [else                                       (Mstate-assign var-name
                                                                 (car maybe-expr)
                                                                 (Mstate-decl var-name
                                                                              null
                                                                              state))])))

;; assigns the value resulting from evaluating expr
;;  onto the state resulting from evaluating expr
(define Mstate-assign
  (lambda (var-name expr state)
    ; correct but redundant work.
    ; can use let or lambda to remove redundant work
    (state-assign-var var-name
                      (Mvalue expr state) ; value result of evaluating expr
                      (Mstate-expr expr state)))) ; state after evaluating expr


;; retrieves value of var from state
;; throws appropriate errors if undeclared or undefined
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

;; returns the value of an expression, in the context of the given state
(define Mvalue
  (lambda (expr state)
    (cond
      [(null? expr)                        (error "called Mvalue on a null expression")]
      [(not (list? expr))                  (Mvalue-base expr state)]
      ; else non-empty list, nested expr
      [(action-is-op? (action expr))       (Mvalue-op (action expr)
                                                      (cdr expr)
                                                      state)]
      [(action-is-assign? (action expr))   (Mvalue (assign-expr expr) state)])))


(define Mbool
  (lambda (expr state)
    (assert-bool (Mvalue expr state))))


; error if not bool, else allows through
(define assert-bool
  (lambda (val)
    (if (boolean? val)
        val
        (error "not a bool"))))
             
     ; ( (x == 1) or (x < 1) 
;; takes an (assumed to be valid) op-symbol and a list of its parameters
;; evals the params in order of the op's associativity
;; and returns the resulting value
(define Mvalue-op
  (lambda (op-symbol param-list state)
    (Mvalue-op-helper op-symbol
                      (map-expr-list-to-value-list (sort-list-to-associativity-of-op op-symbol
                                                                                     param-list)
                                                   state))))

;; fulfills the role of `let`.
;; i.e., not rewriting the lengthy expression above for every case
;; takes an op-symbol and a val-list * already in order of associativity
;; returns the value of the op applied to the list of values
(define Mvalue-op-helper
  (lambda (op-symbol val-list)
    (cond
      [(eq? 1 (length val-list))                  (op-of-symbol op-symbol)]
      ; check types
      ; check # params
      )))

;; returns the state resulting from evaluating
;; the given list of expressions in order of the given op
(define Mstate-op
  (lambda (op-symbol param-list state)
    (state-result-of-expr-list (sort-list-to-associativity-of-op op-symbol param-list) state)))


;; takes an operator and a list of params
;; returns a list of the same params in order
;; of associativity, according to the given op
(define sort-list-to-associativity-of-op
  (lambda (op-symbol lis)
    ;; eval in order, pass state along
    lis)) ; all given ops are left associative



;; returns the state resulting from
;; evaluating the list of expressions
;; in the order they were given
(define state-result-of-expr-list
  (lambda (expr-list state)
    (if (null? expr-list)
        state
        (state-result-of-expr-list (cdr expr-list)
                                   (Mstate-expr (car expr-list) state)))))

;; takes a list of exprs and maps them to values,
;; passing along the updated states
(define map-expr-list-to-value-list
  (lambda (expr-list state)
    (if (null? expr-list)
        expr-list
        (cons (Mvalue (car expr-list) state)
              (map-expr-list-to-value-list (cdr expr-list)
                                           (Mstate-expr (car expr-list) state))))))



;; returns the value of the token given the state
;; token = 1 | 'x | 'true
(define Mvalue-base
  (lambda (token state)
    (cond
      [(number? token)            token]
      [(eq? 'true token)          #t]
      [(eq? 'false token)         #f]
      [else                       (read-var token state)])))


