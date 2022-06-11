#lang racket/base

; A derivative of the parsers given in the CSDS345 assignment,
; refactored to modularize 

(require "lex.rkt"
         racket/list)

(module+ test
  (require rackunit)
  (define-check (check-parse text parse-fn expected-AST)
    ; append whitespace so that lexer produces EOF token. otherwise error
    (start-lex (open-input-string (string-append text " ")))
    (check-equal? (parse-fn) expected-AST)
    (end-lex))
  (define-check (check-parse-exn text parse-fn)
    (start-lex (open-input-string (string-append text " ")))
    (check-exn exn:fail? parse-fn)
    (end-lex)))

; Every non-terminal has a no-arg parse function
; returns false or error on reject
; does not consume tokens if it return false

; parse functions can be combined with union or concat

; union applies first accepting parse function
; within a union, each 'row' must accept a unique start sequence

; concat rejects if first parse function reject,
;failure in subsequent sections should fail
; merges results of each

; 


; ; composition
; list of
; concat
; union
; literal

; add a guard clause to a parse-proc
(define ((of accept-proc parse-proc))
  (and (accept-proc (peek-next-symbol))
       (parse-proc)))

; union of parse functions, the first to accept is applied
; if none accept, runs else-proc
(define ((U #:else-proc [else-proc (λ () #F)] . parsers))
  (or (ormap (λ (p) (p)) parsers)
      (else-proc)))

; a series of parse constructs in serie
; rejects if first parser rejects
; if parsers 2-N reject, they should produce error
(define ((++ #:merge-proc merge-proc
             parser
             . parsers))
  (let ([first-token  (parser)])
    (if first-token
        (apply merge-proc first-token (map (λ (p) (p)) parsers))
        #F)))

; for tokens such as comma, semicolon
(define (of-literal v)
  (of (first-eq?/p v)
      get-car-next))

; empty - consume no tokens, accept any token, produces epsi
; if used in a union, must be terminal case
(define epsi '|epsilon symbol|)
(define (epsi? val) (eq? epsi val))
(define (epsilon) epsi)

; parse-fn else error
(define (required parse-fn msg)
  (U parse-fn #:else-proc (λ () (error 'parse msg))))

; parse-fn else epsilon
(define (? parse-fn)
  (U parse-fn #:else-proc epsilon))

;;;; Predicate builders
(define intersection/p
  (lambda preds
    (lambda (e)
      (andmap (λ (p) (p e)) preds))))

(define (excluding/p predicate . exclude-preds)
  (lambda (e)
    (and (predicate e)
         (not (ormap (λ (p) (p e)) exclude-preds)))))


(define ((first-eq?/p expected) token)
  (eq? expected (car token)))

(define ((member?/p vals) token)
  (member (car token) vals))


;;;; Parse-proc helper
(define (get-car-next)
  (car (get-next-symbol)))
(define (get-cdr-next)
  (cdr (get-next-symbol)))


;;;;

(define keywords '(var if else while try catch finally function 
                       class static new extends throw return continue break))

(define keyword? (member?/p keywords))

(define unary-ops '(- !))
(define binary-ops '(+ - * / %  == != < <= > >=  \|\| &&))


(define p:eof (of-literal 'EOF))
(define p:comma (of-literal 'COMMA))
(define p:=
  (of (λ (token)
        (and (eq? (car token) 'BINARY-OP)
             (eq? (cdr token) '=)))
      get-cdr-next))
(define p:lparen (of-literal 'LEFTPAREN))
(define p:rparen (of-literal 'RIGHTPAREN))

#|
<KEYWORD> => this | super | var | if | else | while
           | try | catch | finally | function | class
           | static | new | extends | throw | return | continue | break

<NAME>    => [a-Z]
           | [a-Z]<NAME>
<IMNAME>  => <NAME> &! [this | super]
|#
(define p:simple-name
  (of (first-eq?/p 'ID)
      get-cdr-next))


;<UN OP>   => [!-]
(define p:unary-op
  (of (intersection/p (first-eq?/p 'BINARY-OP)
                      (member?/p unary-ops))
      get-cdr-next))

;<BI OP>   => [+-*/% == != > >= < <= || &&]
(define p:binary-op
  (of (intersection/p (first-eq?/p 'BINARY-OP)
                      (member?/p binary-ops))
      get-cdr-next))


;<BOOL-LIT>=> true
;           | false
(define p:boolean-literal
  (of (first-eq?/p 'BOOLEAN)
      get-cdr-next))

(module+ test
  (check-parse "true" p:boolean-literal 'true)
  (check-parse "false" p:boolean-literal 'false)
  (check-parse "5" p:boolean-literal #F))

;<DIGIT>   => [0-9]
;<DIGITS>  => epsi
;           | <DIGIT><DIGITS>
;<LEADDIG> => [1-9]
;<UNUMBER> => <LEADDIG><DIGITS>
(define p:unsigned-number
  (of (first-eq?/p 'NUMBER)
      get-cdr-next))

(module+ test
  (check-parse "0" p:unsigned-number 0)
  (check-parse "11" p:unsigned-number 11)
  (check-parse "151" p:unsigned-number 151)
  (check-parse "true" p:unsigned-number #F))

;<NEGATE-E>=> !<NEG-VAL> | !<FUNCALLE> | !<CNAME>
;<NEG-VAL> => <BOOLE> | <FUNCALLE> | <CNAME>
;<BOOLE>   => <BOOL-LIT> | <NEGATE-E> | <BOOLE> <BOOL-BI-OP> <BOOLE>
;(define unary-expr
;  (concat-parsers p:unary-op p:value))


#| todo: operator precedence. also ambiguous union of number and V BOP V
<VALUE>   => <CNAME>
           | (<VALUE>)
           | <NUMBER>
           | <BOOL-LIT>
           | <FUNCALLE>
           | <VALUE> <BINARY OP> <VALUE>
           | <UNARY OP> <U-VALUE>
           | <ASSIGNE>
           | <NEW>

<VALUE>   => <V><SUFF>
           | <UNARY OP>
           | (<VALUE>)

<V>       =>
<SUFF>    => <BINARY OP> <VALUE>
           | . <DOT RHS>
           | 
           | epsi

precedence from highest to lowest
(object.member [] (args) post++ post--) (!~ pre++ pre-- unary+-) (new) (*/%)
 (binary+-) (<< >> >>>) (< <= > >=) (== !=) (&) (^) (|) (&&) (||) (?:) (=) (->)
|#
(define p:value
  (U p:unsigned-number
     p:simple-name
     p:boolean-literal))


;<ASSIGNE>  => <CNAME> = <VALUE>
(define p:assign-expr
  (++ p:simple-name
      p:=
      p:value
      #:merge-proc (λ (LHS = v) (list = LHS v))))

(module+ test
  (check-parse "x = 2" p:assign-expr '(= x 2)))

;;;; Statements

(define p:var-kw (of-literal 'var))
(define p:function-kw (of-literal 'function))

(define p:semicolon (of-literal 'SEMICOLON))
(define p:required-semicolon (required p:semicolon "missing semicolon"))
(define p:lbrace (of-literal 'LEFTBRACE))
(define p:rbrace (of-literal 'RIGHTBRACE))

;<break>    => break;
(define p:break
  (++ (of-literal 'break)
      p:required-semicolon
      #:merge-proc (λ (_ semicolon)
                     '(break))))

(module+ test
  (check-parse "break;" p:break '(break))
  (check-parse-exn "break" p:break)
  (check-parse "return 5;" p:break #F))

;<continue> => continue;
(define p:continue
  (++ (of-literal 'continue)
      p:required-semicolon
      #:merge-proc (λ (_ semicolon) '(continue))))

(module+ test
  (check-parse "continue;" p:continue '(continue))
  (check-parse-exn "continue" p:continue)
  (check-parse "return 5;" p:continue #F))


;<ASSIGNS>  => <ASSIGNE>;
(define p:assign-stmt
  (++ p:assign-expr
      p:required-semicolon
      #:merge-proc (λ (assign-AST semicolon) assign-AST)))

(module+ test
  (check-parse "x = 2;" p:assign-stmt '(= x 2))
  (check-parse-exn "x = 2" p:assign-stmt)
  (check-parse "return 5;" p:assign-stmt #F))


;<RETURN>   => return <VALUE>;
(define p:return
  (++ (of-literal 'return)
      (required p:value "missing value")
      p:required-semicolon
      #:merge-proc (λ (ret-kw v semicolon) (list ret-kw v))))

(module+ test
  (check-parse "return 2;" p:return '(return 2))
  (check-parse-exn "return;" p:return)
  (check-parse-exn "return 5" p:return)
  (check-parse "x = 5;" p:return #F))

;<THROW>    => throw <VALUE>;
(define p:throw
  (++ (of-literal 'throw)
      (required p:value "missing value")
      p:required-semicolon
      #:merge-proc (λ (throw-kw v semicolon) (list throw-kw v))))

(module+ test
  (check-parse "throw 2;" p:throw '(throw 2))
  (check-parse-exn "throw;" p:throw)
  (check-parse-exn "throw 5" p:throw)
  (check-parse "x = 5;" p:throw #F))

#|
<BRNCHBDY>=> <ASSIGN> | <FUNCALL>
           | <IF>
           | <WHILE>
           | <BLOCK>
|#

;<ELSEBODY>=> <BRNCHBDY>

;<IF>      => if (<COND>) <BRNCHBDY>
;           | if (<COND>) <BRNCHBDY> else <ELSEBODY>
(define p:\(value\)
  (++ p:lparen
      p:value
      p:rparen
      #:merge-proc (λ (\( val \)) val)))

(define (p:if)
  (define p:if-body
    (U p:assign-stmt
       p:if
       p:while
       ;p:block
       ))
  ((++ (of-literal 'if)
       p:\(value\)
       p:if-body
       (? (++ (of-literal 'else)
              (p:if-body)
              #:merge-proc (λ (else-kw else-body) else-body)))
       #:merge-proc (λ (if-kw cond-val then-body opt-else)
                      (cons if-kw (cons cond-val (cons then-body (if (epsi? opt-else)
                                                                     '()
                                                                     (list opt-else)))))))))

;<WHILE>   => while (<COND>) <BRNCHBDY>
(define p:while-body 0)
(define p:while
  (++ (of-literal 'while)
      p:\(value\)
      p:while-body
      #:merge-proc (λ (while-kw cond-val while-body)
                     (list while-kw cond-val while-body))))

;<CLASS>   => class <NAME> { <CLASSBODY> }
;           | class <NAME> extends <NAME> { <CLASSBODY> }
(define p:class-body 'todo)
(define p:class
  (++ (of-literal 'class)
      p:simple-name
      (? (++ (of-literal 'extends)
             p:simple-name
             #:merge-proc (λ (ext-kw parent-name) parent-name)))
      p:lbrace
      p:class-body
      p:rbrace
      #:merge-proc (λ (class-kw class-name opt-parent \{ class-body \})
                     (list 'class class-name (if (epsi? opt-parent) '() opt-parent) class-body))))

#|
<DLVALUE>  => <FUNCALLE>
            | <NEW>
            | <CNAME>

<CNAME>    => <NAME>
            | <DLVALUE>.<NAME>


<METLHS>   => this
            | super
            | <NEW>
            | <CNAME>
            | <FUNCALLE>
<FNCLHS>   => <NAME>
            | <METLHS>.<NAME>
<FUNCALLE> => <FNCLHS>(<ARGS>)


<NEW>      => new <NAME>(<ARGS>)


<FUNCALLS> => <FUNCALLE>;

<VAR>      => var <NAME>;
            | var <NAME> = <VALUE>;


<STVAR>    => static var <NAME>;
            | static var <NAME> = <VALUE>


<TRY>     => try {} catch(<name>) {} finally {}
           | try {} catch(<name>) {}
           | try {} finally {}

<BLOCK>   => { <STMTS> }
<STMTS>   => epsi
           | <STMT><STMTS>

! this and super can't be in static contexts -> static fun, static var decl

<FNBODY>  => epsi
           | <FSTMT><FNBODY>

<FUNDEF>  => function <NAME>(<PARAMS>) { <FUNCTIONBODY> }
<ABFNDEF> => function <NAME>(<PARAMS>);
<STFNDEF> => static function <NAME>(<PARAMS>) { <FUNCTIONBODY> }

<CTORDEF> => <NAME>(<PARAMS>)

|#

; since parser works LtR, have to use branching for look-ahead
; consider `var a = x;`  vs  `var a = x();
; `var (id . a) (bi op . =) (id . x) SEMICOLON`
; vs
; `var (id . a) (bi op . =) (id . x) (          ) SEMICOLON`

; (var {name} {val}

(define (parse-port input-port)
  (begin
    (start-lex input-port)
    (let ([parse-tree (program-parse)])
      (end-lex)
      parse-tree)))

(define (program-parse)
  '(0))

; takes a program, returns list of the lexed tokens
(define (showlex str)
  (print str) (display "\n->\n") (println (lex-all str)) (displayln ""))
(define (lex-all str)
  (start-lex (open-input-string str))
  (lexallloop))

(define (lexallloop)
  (let ([token  (get-next-symbol)])
    (cons token (if (eq? (car token) 'EOF)
                    null
                    (lexallloop)))))

;(showlex "var x.y = this.x; function super(this.abc(a,b));
;this super var if else while try catch finally function class static new extends throw return continue break ")



