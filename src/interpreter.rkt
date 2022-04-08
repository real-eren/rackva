#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSDS 343 Interpreter
;; 2022 Spring
;; Duc Huy Nguyen, Eren Kahriman, Loc Nguyen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "conts.rkt"
         "state.rkt"
         "util/map.rkt"
         "functionParser.rkt")

(provide interpret
         interpret-parse-tree
         simple-interpret-parse-tree)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; High-level functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; takes a file name, interprets the program,
;; and returns the result
(define interpret
  (lambda (file-name)
    (interpret-parse-tree (parser file-name))))

(define interpret-parse-tree
  (lambda (parse-tree)
    (Mstate-top-level parse-tree
                      (state:push-stack-trace 'top-level new-state)
                      (conts-of
                       #:return (lambda (v s) (myerror "return as top-level statement" s))
                       #:next (lambda (s) (Mstate-main (state:pop-stack-trace s)))
                       #:throw default-throw
                       #:break (lambda (s) (myerror "break as top-level statement" s))
                       #:continue (lambda (s) (myerror "continue as top-level statement" s))))))

;; legacy, for testing
;; interprets programs accepted by simpleParser.rkt
(define simple-interpret-parse-tree
  (lambda (simple-parse-tree)
    (Mstate-stmt-list simple-parse-tree
                      new-state
                      (conts-of
                       #:return (lambda (v s) (prep-val-for-output v))
                       #:next (lambda (s) (myerror "reached end of program without return" s))
                       #:throw default-throw
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

;; Evaluates all the top level statements
(define Mstate-top-level
  (lambda (stmt-list state conts)
    (if (null? stmt-list)
        ((next conts) state)
        (Mstate-top-level-stmt (car stmt-list)
                               state
                               (conts-of conts
                                         #:next (lambda (s)
                                                  (Mstate-top-level (cdr stmt-list)
                                                                    s
                                                                    conts))
                                         #:throw default-throw)))))

;; Evaluates a single top level statement
(define Mstate-top-level-stmt
  (lambda (statement state conts)
    (if (or (is-declare? statement)
            (is-assign? statement)
            (is-fun-decl? statement))
        (Mstate-statement statement state conts)
        (myerror "illegal top-level statement: " statement))))

;; Find and execute the main function with the initial state
; should be run after executing all other top-level statements
(define Mstate-main
  (lambda (state)
    (Mvalue-fun '(funcall main)
                state
                (conts-of
                 #:next (lambda (s) (myerror "main function terminated without returning"))
                 #:throw default-throw)
                (lambda (v s) (prep-val-for-output v)))))


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
      [else                                 (myerror (format "unrecognized stmt: #s" statement) state)])))


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
                      (conts-of conts
                                #:next (lambda (s)
                                         ((next conts) (state:pop-layer s)))
                                #:break (lambda (s)
                                          ((break conts) (state:pop-layer s)))
                                #:continue (lambda (s)
                                             ((continue conts) (state:pop-layer s)))
                                #:throw (lambda (v s)
                                          ((throw conts) v (state:pop-layer s)))
                                #:return (lambda (v s)
                                           ((return conts) v (state:pop-layer s)))))))


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
                                                                         conts))
                                             #:break (next conts)
                                             #:continue (lambda (s3)
                                                          (Mstate-while-impl condition
                                                                             body
                                                                             s3
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
; invokes the return continuation
(define Mstate-return
  (lambda (statement state conts)
    (Mvalue (return-expr-part statement)
            state
            conts
            (lambda (v s)
              ((return conts) v s)))))


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
            conts
            (lambda (v s)
              ((throw conts) v s)))))


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
        (myerror (string-append "attempted to re-declare function "
                                (symbol->string fun-name))
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
      [(state:var-declared-top-frame? var-name state)   (myerror (string-append "attempted to re-declare "
                                                                                (symbol->string var-name))
                                                                 state)]
      [(null? maybe-expr)                               ((next conts) (state:declare-var var-name state))]
      [else                                             (Mvalue (get maybe-expr)
                                                                state
                                                                conts
                                                                (lambda (v s)
                                                                  ((next conts) (state:declare-var-with-value var-name 
                                                                                                              v
                                                                                                              s))))])))


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
        (Mvalue val-expr state conts (lambda (v s)
                                       ((next conts) (state:assign-var var-name v s))))
        ; else assigning to undeclared var
        (myerror (string-append "tried to assign to "
                                (symbol->string var-name)
                                " before declaring it.")
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
                                                                                         s
                                                                                         (conts-of conts
                                                                                                   #:next (lambda (s2)
                                                                                                            ((throw conts) e s2)))))]
                                           [(null? finally-block)   (lambda (e s)
                                                                      (Mstate-block-impl (catch-body catch-block)
                                                                                         (state:declare-var-with-value (catch-var catch-block)
                                                                                                                       e
                                                                                                                       s)
                                                                                         conts))]
                                           [else                    (lambda (e s)
                                                                      (Mstate-block-impl (catch-body catch-block)
                                                                                         (state:declare-var-with-value (catch-var catch-block)
                                                                                                                       e
                                                                                                                       s)
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

;; takes an expression and evaluate it
;; error if not bool
(define Mbool
  (lambda (expr state conts evaluate)
    (cond
      [(not (nested? expr))      (Mvalue-base expr
                                              state
                                              conts
                                              (lambda (v s)
                                                (evaluate (assert-bool v) s)))]
      [(is-||? expr)             (Mbool (bool-left-op expr)
                                        state
                                        conts
                                        (lambda (b1 s1)
                                          (if b1
                                              (evaluate b1 s1)
                                              (Mbool (bool-right-op expr)
                                                     s1
                                                     conts
                                                     (lambda (b2 s2)
                                                       (evaluate b2 s2))))))]
      [(is-&&? expr)             (Mbool (bool-left-op expr)
                                        state
                                        conts
                                        (lambda (b1 s1)
                                          (if (not b1)
                                              (evaluate b1 s1)
                                              (Mbool (bool-right-op expr)
                                                     s1
                                                     conts
                                                     (lambda (b2 s2)
                                                       (evaluate b2 s2))))))]
      [(is-assign? expr)         (Mvalue expr
                                         state
                                         conts
                                         (lambda (b s)
                                           (evaluate (assert-bool b) s)))]
      [(is-nested-boolean-expr?
        expr)                    (Mvalue-op expr
                                            state
                                            conts
                                            (lambda (b s)
                                              (evaluate (assert-bool b) s)))]
      [(is-fun? expr)            (Mvalue-fun expr 
                                             state 
                                             conts 
                                             (lambda (b s)
                                               (evaluate (assert-bool b) s)))]
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
  (lambda (expr state conts evaluate)
    (cond
      [(null? expr)                         (error "called Mvalue on a null expression")]
      [(not (nested? expr))                 (Mvalue-base expr state conts evaluate)]
      [(is-fun? expr)                       (Mvalue-fun expr state conts evaluate)]
      [(is-assign? expr)                    (Mvalue (assign-expr expr) 
                                                    state
                                                    conts
                                                    (lambda (v s) 
                                                      (evaluate v (state:assign-var (assign-var expr) v s))))]
      [(is-nested-boolean-expr? expr)       (Mbool expr state conts evaluate)]
      [(has-op? expr)                       (Mvalue-op expr state conts evaluate)]
      [else                                 (error "unreachable in Mvalue")])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Mvalue-fun
  (lambda (expr state conts evaluate)
    (Mshared-fun expr
                 state
                 (conts-of conts
                           #:next      (lambda (s) (myerror "The function did not return any values!" s))
                           #:return    (lambda (v s) (evaluate v state))))))

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
                                   #:throw    (lambda (e s) ((throw conts) e state))))
        (myerror (string-append "function "
                                (symbol->string (fun-name expr))
                                " not in scope.")
                 state))))


(define Mvalue-fun-impl
  (lambda (fun-name fun-closure fun-inputs state conts)
    (get-environment fun-name 
                     fun-closure
                     fun-inputs
                     (state:push-stack-trace fun-name state)
                     conts
                     (lambda (s)
                       (Mstate-stmt-list (closure:body fun-closure) 
                                         s
                                         conts)))))


;; Takes in the function name, the function closure, the input expression
;; the state, the conts and the evaluation
;; It should evaluate the state that should be used during the function call
(define get-environment
  (lambda (fun-name fun-closure inputs state conts next)
    (get-inputs-list-box-cps (closure:params fun-closure)
                             inputs
                             state
                             conts
                             (lambda (p l s)
                               (bind-boxed-params p
                                                  l
                                                  ((closure:scoper fun-closure) state)
                                                  (lambda (s2)
                                                    (next (state:declare-fun fun-name
                                                                             (closure:params   fun-closure)
                                                                             (closure:body     fun-closure)
                                                                             (closure:scoper   fun-closure)
                                                                             (state:push-new-layer s2)))))))))


;; Takes in the inputs and params and the current state, return the mapping of params and values
;; The evaluation passing the list of boxes of input, the params without the & and the new state 
(define get-inputs-list-box-cps
  (lambda (formal-params actual-params state conts evaluation) 
    (cond
      [(and (null? actual-params)
            (null? formal-params))      (evaluation '() '() state)]
      [(null? actual-params)            (myerror "Too few inputs for function call!" state)]
      [(null? formal-params)            (myerror "Too many inputs for function call!" state)]
      ;; by value
      [(not (eq? (car formal-params)
                 '&))                   (Mvalue (car actual-params)
                                                state
                                                conts
                                                (lambda (v1 s1)
                                                  (get-inputs-list-box-cps (cdr formal-params)
                                                                           (cdr actual-params)
                                                                           s1
                                                                           conts
                                                                           (lambda (p1 l1 s2)
                                                                             (evaluation (cons (car formal-params) p1) 
                                                                                         (cons (box v1) l1) 
                                                                                         s2)))))]
      ;; by reference
      [(symbol? (car actual-params))    (get-inputs-list-box-cps (cddr formal-params)
                                                                 (cdr actual-params)
                                                                 state
                                                                 conts
                                                                 (lambda (p b s)
                                                                   (evaluation (cons (second formal-params) p)
                                                                               (cons (state:get-var-box (car actual-params) state) b)
                                                                               s)))]
      [else                             (myerror (string-append "Function requires a reference for "
                                                                (symbol->string (second formal-params)))
                                                 state)])))

;; Binds the names of the formal-params to boxes representing the actual parameters
(define bind-boxed-params
  (lambda (formal-params box-list state next)
    (if (null? box-list)
        (next state)
        (bind-boxed-params (cdr formal-params) 
                           (cdr box-list)
                           state
                           (lambda (s)
                             (next (state:declare-var-with-box (car formal-params) (car box-list) s)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXPRESSIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns the value of the token given the state
;; token = 1 | 'x | 'true 
(define Mvalue-base
  (lambda (token state conts evaluate)
    (cond
      [(number? token)            (evaluate token state)]
      [(eq? 'true token)          (evaluate #t state)]
      [(eq? 'false token)         (evaluate #f state)]
      [(symbol? token)            (evaluate (read-var token state) state)]
      [else                       (error "not a bool/int literal or symbol: " token)])))


;; retrieves value of a var from state
;; throws appropriate errors if undeclared or uninitialized
(define read-var
  (lambda (var-symbol state)
    (cond
      [(not (state:var-declared? var-symbol state))       (myerror (string-append "referenced "
                                                                                  (symbol->string var-symbol)
                                                                                  " before declaring it.")
                                                                   state)]
      [(not (state:var-initialized? var-symbol state))    (myerror (string-append "accessed "
                                                                                  (symbol->string var-symbol)
                                                                                  " before initializing it.")
                                                                   state)]
      [else                                               (state:get-var-value var-symbol state)])))


;; takes an expression representing one with an operator
; ex: -(expr) | !(bool-expr) | -x | x+y | z/2 | etc
(define op-op-symbol first)
(define op-param-list cdr)

;; takes a nested expression containing an op
;; and evaluates it
(define Mvalue-op
  (lambda (expr state conts evaluate)
    (map-expr-list-to-value-list (op-param-list expr)
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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; functions that may be useful in more than one context

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
(define is-fun-decl? (checker-of 'function))
(define is-fun? (checker-of 'funcall))

;; keys are symbols representative of a construct type
;; values are the corresponding constructs (type of statement)
(define constructs-table
  (map:from-interlaced-entries
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
  (map:from-interlaced-entries
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
  (map:from-interlaced-entries
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

(define default-throw (lambda (v s) (myerror (format "uncaught exception: ~s" v) s)))
(define default-break (lambda (s) (myerror "break statement outside of loop" s)))
(define default-continue (lambda (s) (myerror "continue statement outside of loop" s)))

