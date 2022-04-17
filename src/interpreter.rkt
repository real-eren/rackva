#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSDS 345 Interpreter
;; 2022 Spring
;; Duc Huy Nguyen, Eren Kahriman, Loc Nguyen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "conts.rkt"
         "state/state.rkt"
         "util/map.rkt"
         "util/predicates.rkt"
         "functionParser.rkt"
         ;"classParser.rkt"
         )

(provide interpret
         interpret-parse-tree-v3
         interpret-parse-tree-v2
         interpret-parse-tree-v1

         default-return
         default-throw)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; High-level functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; takes a file name, interprets the program,
;; and returns the result
(define interpret
  (lambda (file-name)
    (interpret-parse-tree-v2 (parser file-name)
                             default-return
                             default-throw)))
;todo: consider refactoring interpret-parse-tree s.t. they share 
;; interprets parse-trees produced by classParser.rkt
(define interpret-parse-tree-v3
  (lambda (parse-tree entry-point return throw)
    (Mstate-stmt-list parse-tree
                      (state:push-stack-trace 'top-level new-state)
                      (conts-of
                       ; lookup entry-point class (name), do Mstate-main w/ popped stack-trace and forwarded return & throw
                       #:next (lambda (s) (string->symbol entry-point))
                       )
                      #:legal-construct? (is-class-decl?))))

;;interprets parse-trees produced by functionParser.rkt
(define interpret-parse-tree-v2
  (lambda (parse-tree return throw)
    (Mstate-stmt-list parse-tree
                      (state:push-stack-trace 'top-level new-state)
                      (conts-of ; only next and throw are actually reachable
                       #:return (lambda (v s) (myerror "return as top-level statement" s))
                       #:next (lambda (s) (Mstate-main (state:pop-stack-trace s) return throw))
                       #:throw throw
                       #:break (lambda (s) (myerror "break as top-level statement" s))
                       #:continue (lambda (s) (myerror "continue as top-level statement" s)))
                      #:legal-construct? (join|| is-declare? is-assign? is-fun-decl?))))

;; legacy, for testing
;; interprets parse-trees produced by simpleParser.rkt
(define interpret-parse-tree-v1
  (lambda (simple-parse-tree return throw)
    (Mstate-stmt-list simple-parse-tree
                      new-state
                      (conts-of
                       #:return return
                       #:next (lambda (s) (raise-user-error "reached end of program without return"))
                       #:throw throw
                       #:break default-break
                       #:continue default-continue))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TOP-LEVEL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Find and execute the main function with the initial state
; should be run after executing all other top-level statements
(define Mstate-main
  (lambda (state return throw)
    (Mvalue-fun '(funcall main)
                state
                (conts-of
                 #:throw throw
                 #:return return))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; STATEMENT LIST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; executes a list of statements with a given initial state
;; and returns the resulting state
; optionally takes a filter function (on statement) and throws an error if an
; encountered statement does not pass the filter
(define Mstate-stmt-list
  (lambda (statement-list state conts #:legal-construct? [legal? (λ (v) #T)])
    (cond
      [(null? statement-list)         ((next conts) state)]
      [(legal? (car statement-list))  (Mstate-statement (car statement-list)
                                                        state
                                                        (conts-of conts
                                                                  #:next (lambda (s)
                                                                           (Mstate-stmt-list (cdr statement-list) 
                                                                                             s
                                                                                             conts))))]
      ; indicative of bug in program
      [else                           (error "encountered illegal construct type" (car statement-list))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; STATEMENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes a statement
;; returns the state resulting from evaluating it
(define Mstate-statement
  (lambda (statement state conts)
    (cond
      [(null? statement)                    (error "called Mstate on null statement")]
      [(is-construct? statement)            ((get-Mstate statement) statement state conts)]
      [else                                 (error (format "unrecognized stmt: ~a" statement))])))


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
            (conts-of conts #:return (throw conts)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;   FUNCTION DECLARATION   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; takes a statement representing a function declaration statement
; declare the function in the state

(define decl-fun-name second)
(define decl-fun-params third)
(define decl-fun-body fourth)

(define Mstate-decl-fun
  (lambda (statement state conts)
    (Mstate-decl-fun-impl (decl-fun-name statement) 
                          (decl-fun-params statement) 
                          (decl-fun-body statement) 
                          state
                          conts)))

(define Mstate-decl-fun-impl
  (lambda (fun-name fun-params fun-body state conts)
    (if (state:has-fun? fun-name state)
        (myerror (format "function `~s` is already declared in the current scope."
                         fun-name)
                 state)
        ((next conts) (state:declare-fun fun-name 
                                         fun-params
                                         fun-body
                                         (state:make-scoper state)
                                         state)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;   FUNCTION INVOCATION   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; takes a statement representing a function statement
; invokes the function

(define fun-name second)
(define fun-inputs cddr)

(define Mstate-fun
  (lambda (expr state conts)
    (Mshared-fun expr
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
      [(state:var-declared-top-frame? var-name state)   (myerror (format "`~a` is already declared in the current scope."
                                                                         var-name)
                                                                 state)]
      [(null? maybe-expr)                               ((next conts) (state:declare-var var-name state))]
      [else                                             (Mvalue (get maybe-expr)
                                                                state
                                                                (conts-of conts
                                                                          #:return (lambda (v s)
                                                                                     ((next conts) (state:declare-var-with-value var-name 
                                                                                                                                 v
                                                                                                                                 s)))))])))


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
    (if (state:var-declared? var-name state)
        (Mvalue val-expr
                state
                (conts-of conts
                          #:return (lambda (v s)
                                     ((next conts) (state:assign-var var-name v s)))))
        ; else assigning to undeclared var
        (myerror (format "tried to assign to `~a` before declaring it."
                         var-name)
                 state))))

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
    (Mstate-block-impl try-body
                       state
                       (conts-of conts
                                 #:next (if (null? finally-block)
                                            (next conts)
                                            (lambda (s)
                                              (Mstate-block-impl (finally-body finally-block)
                                                                 s
                                                                 conts)))
                                 #:break (if (null? finally-block)
                                             (break conts)
                                             (lambda (s)
                                               (Mstate-block-impl (finally-body finally-block)
                                                                  s
                                                                  (conts-of conts
                                                                            #:next (break conts)))))
                                 #:continue (if (null? finally-block)
                                                (continue conts)
                                                (lambda (s)
                                                  (Mstate-block-impl (finally-body finally-block)
                                                                     s
                                                                     (conts-of conts
                                                                               #:next (continue conts)))))
                                 #:return (if (null? finally-block)
                                              (return conts)
                                              (lambda (v s)
                                                (Mstate-block-impl (finally-body finally-block)
                                                                   s
                                                                   (conts-of conts
                                                                             #:next (lambda (s2)
                                                                                      ((return conts) v s2))))))
                                 #:throw (cond
                                           [(null? catch-block)     (lambda (e s)
                                                                      (Mstate-block-impl (finally-body finally-block)
                                                                                         (state:with-stack-trace (state:stack-trace state) s)
                                                                                         (conts-of conts
                                                                                                   #:next (lambda (s2)
                                                                                                            ((throw conts) e (state:with-stack-trace (state:stack-trace s) s2))))))]
                                           [(null? finally-block)   (lambda (e s)
                                                                      (Mstate-block-impl (catch-body catch-block)
                                                                                         (state:declare-var-with-value (catch-var catch-block)
                                                                                                                       e
                                                                                                                       (state:with-stack-trace (state:stack-trace state) s))
                                                                                         conts))]
                                           [else                    (lambda (e s)
                                                                      (Mstate-block-impl (catch-body catch-block)
                                                                                         (state:declare-var-with-value (catch-var catch-block)
                                                                                                                       e
                                                                                                                       (state:with-stack-trace (state:stack-trace state) s))
                                                                                         (conts-of conts ; after catch, before finally
                                                                                                   #:next (lambda (s2)
                                                                                                            (Mstate-block-impl (finally-body finally-block)
                                                                                                                               s2
                                                                                                                               conts))
                                                                                                   #:break (lambda (s2)
                                                                                                             (Mstate-block-impl (finally-body finally-block)
                                                                                                                                s2
                                                                                                                                (conts-of conts
                                                                                                                                          #:next (break conts))))
                                                                                                   #:continue (lambda (s2)
                                                                                                                (Mstate-block-impl (finally-body finally-block)
                                                                                                                                   s2
                                                                                                                                   (conts-of conts
                                                                                                                                             #:next (continue conts))))
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
                                                                                                                                                    ((return conts) v s3))))))))])))))



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
    (Mbool-impl expr state (w/preproc conts #:map-value assert-bool))))

(define Mbool-impl
  (lambda (expr state conts)
    (cond
      [(not (nested? expr))      (Mvalue-base expr state conts)]
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
      [(is-fun? expr)            (Mvalue-fun expr state conts)]
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
      [(null? expr)                         (error "called Mvalue on a null expression")]
      [(not (nested? expr))                 (Mvalue-base expr state conts)]
      [(is-fun? expr)                       (Mvalue-fun expr state conts)]
      [(is-assign? expr)                    (Mvalue (assign-expr expr)
                                                    state
                                                    (conts-of conts
                                                              #:return (lambda (v s) 
                                                                         ((return conts) v (state:assign-var (assign-var expr) v s)))))]
      [(is-nested-boolean-expr? expr)       (Mbool expr state conts)]
      [(has-op? expr)                       (Mvalue-op expr state conts)]
      [else                                 (error "unreachable in Mvalue")])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Mvalue-fun
  (lambda (expr state conts)
    (Mshared-fun expr
                 state
                 (conts-of conts
                           #:next      (lambda (s) (myerror "The function did not return any values!" s))
                           #:return    (lambda (v s) ((return conts) v state))))))

;; common logic for Mstate-fun and Mvalue-fun
(define Mshared-fun
  (lambda (expr state conts)
    (if (state:has-fun? (fun-name expr) state)
        (Mvalue-fun-impl (fun-name expr)
                         (state:get-closure (fun-name expr) state)
                         (fun-inputs expr)
                         state
                         (conts-of conts
                                   #:continue default-continue
                                   #:break    default-break
                                   #:throw    (lambda (e s) ((throw conts) e (state:with-stack-trace (state:stack-trace s) state)))))
        (myerror (format "function `~a` not in scope"
                         (fun-name expr))
                 state))))


(define Mvalue-fun-impl
  (lambda (fun-name fun-closure fun-inputs state conts)
    (Mstate-stmt-list (closure:body fun-closure)
                      (get-environment fun-name 
                                       fun-closure
                                       fun-inputs
                                       (state:push-stack-trace fun-name state)
                                       conts)
                      conts)))


;; Takes in the function name, the function closure, the input expression
;; the state, the conts
(define get-environment
  (lambda (fun-name fun-closure inputs state conts)
    (if (eq? (closure:num-formal-params fun-closure)
             (length inputs))
        (get-inputs-list-box-cps (closure:params fun-closure)
                                 inputs
                                 state
                                 conts
                                 (lambda (p l s)
                                   (bind-boxed-params p
                                                      l
                                                      (state:push-new-layer ((closure:scoper fun-closure) s)))))
        (myerror (format "`~a` expected ~a argument(s), got ~a."
                         fun-name
                         (closure:num-formal-params fun-closure)
                         (length inputs))
                 state))))

;; returns the number of formal parameters in a closure
(define closure:num-formal-params
  (lambda (closure)
    (length (filter (lambda (p) (not (eq? '& p))) (closure:params closure)))))

;; Takes in the inputs and params and the current state, return the mapping of params and values
;; The evaluation passing the list of boxes of input, the params without the & and the new state 
(define get-inputs-list-box-cps
  (lambda (formal-params actual-params state conts evaluation) 
    (cond
      [(and (null? actual-params)
            (null? formal-params))      (evaluation '() '() state)]
      ;; by value
      [(not (eq? (car formal-params)
                 '&))                   (Mvalue (car actual-params)
                                                state
                                                (conts-of conts
                                                          #:return (lambda (v1 s1)
                                                                     (get-inputs-list-box-cps (cdr formal-params)
                                                                                              (cdr actual-params)
                                                                                              s1
                                                                                              conts
                                                                                              (lambda (p1 l1 s2)
                                                                                                (evaluation (cons (car formal-params) p1) 
                                                                                                            (cons (box v1) l1) 
                                                                                                            s2))))))]
      ;; by reference
      [(symbol? (car actual-params))    (get-inputs-list-box-cps (cddr formal-params)
                                                                 (cdr actual-params)
                                                                 state
                                                                 conts
                                                                 (lambda (p b s)
                                                                   (evaluation (cons (second formal-params) p)
                                                                               (cons (state:get-var-box (car actual-params) s) b)
                                                                               s)))]
      [else                             (myerror (format "Function expects a reference for `~a`"
                                                         (second formal-params))
                                                 state)])))

;; Binds the names of the formal-params to boxes representing the actual parameters
(define bind-boxed-params
  (lambda (formal-params box-list state)
    (foldl state:declare-var-with-box
           state
           formal-params
           box-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXPRESSIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns the value of the token given the state
;; token = 1 | 'x | 'true 
(define Mvalue-base
  (lambda (token state conts)
    ((return conts) (Mvalue-base-impl token state) state)))

(define Mvalue-base-impl
  (lambda (token state)
    (cond
      [(number? token)            token]
      [(eq? 'true token)          #t]
      [(eq? 'false token)         #f]
      [(symbol? token)            (read-var token state)]
      [else                       (error "not a bool/int literal or symbol: " token)])))

;; retrieves value of a var from state
;; throws appropriate errors if undeclared or uninitialized
(define read-var
  (lambda (var-symbol state)
    (cond
      [(not (state:var-declared? var-symbol state))       (myerror (format "referenced `~a` before declaring it."
                                                                           var-symbol)
                                                                   state)]
      [(not (state:var-initialized? var-symbol state))    (myerror (format "accessed `~a` before initializing it."
                                                                           var-symbol)
                                                                   state)]
      [else                                               (state:get-var-value var-symbol state)])))


;; takes an expression representing one with an operator
; ex: -(expr) | !(bool-expr) | -x | x+y | z/2 | etc
(define op-op-symbol first)
(define op-param-list cdr)

;; takes a nested expression containing an op
;; and evaluates it
(define Mvalue-op
  (lambda (expr state conts)
    (map-expr-list-to-value-list (op-param-list expr)
                                 state
                                 (w/preproc conts
                                            #:map-value (lambda (val-list)
                                                          (op-apply (op-of-symbol (op-op-symbol expr)) val-list))))))


;; takes a list of exprs and maps them to values,
;; propagating the state changes (so that they evaluate correctly)
(define map-expr-list-to-value-list
  (lambda (expr-list state conts)
    (if (null? expr-list)
        ((return conts) expr-list state)
        (Mvalue (car expr-list)
                state
                (conts-of conts
                          #:return (lambda (v1 s1)
                                     (map-expr-list-to-value-list (cdr expr-list)
                                                                  s1
                                                                  (conts-of conts
                                                                            #:return (lambda (v2 s2)
                                                                                       ((return conts) (cons v1 v2) s2))))))))))


;; takes an op and a val-list * already in order of associativity
;; returns the value of the op applied to the list of values
(define op-apply
  (lambda (op val-list)
    (cond
      [(eq? 1 (length val-list))                  (op (first val-list))]
      [(eq? 2 (length val-list))                  (op (first val-list) (second val-list))]
      [else                                       (error op val-list)])))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; functions that may be useful in more than one context

;; takes a statement or nested expr (list), returns what should be an action symbol
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
(define is-fun-decl? (checker-of 'function))
(define is-fun? (checker-of 'funcall))
(define is-class-decl? (checker-of 'class))

;; keys are symbols representative of a construct type
;; values are the corresponding constructs (type of statement)
(define constructs-table
  (map:of
   'return   Mstate-return
   'while    Mstate-while
   'if       Mstate-if
   'var      Mstate-declare
   '=        Mstate-assign
   'begin    Mstate-block
   'try      Mstate-try
   'throw    Mstate-throw
   'break    Mstate-break
   'function Mstate-decl-fun
   'funcall  Mstate-fun
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

;; assuming the atom is an op-symbol, returns the associated function
(define op-of-symbol
  (lambda (op-symbol)
    (if (map:contains? op-symbol arithmetic-op-table)
        (map:get op-symbol arithmetic-op-table)
        (map:get op-symbol boolean-op-table))))


;;;;;;; Custom error functions

;; retrieves the stack-trace from state and returns it in a form fit for output
(define formatted-stack-trace
  (lambda (state)
    (if (empty? (state:stack-trace state))
        ""
        (string-join (map symbol->string (reverse (state:stack-trace state)))
                     " -> "
                     #:before-first "stack trace: "))))
;; for user-facing errors
(define myerror
  (lambda (msg state)
    (raise-user-error (string-append msg "\n" (formatted-stack-trace state)))))

;;;;;;;; Common Continuations

(define default-return (lambda (v s) (prep-val-for-output v)))
(define default-throw (lambda (v s) (myerror (format "uncaught exception: ~a" v) s)))
(define default-break (lambda (s) (myerror "break statement outside of loop" s)))
(define default-continue (lambda (s) (myerror "continue statement outside of loop" s)))

