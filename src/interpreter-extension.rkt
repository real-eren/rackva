#lang racket/base

(require "interpreter.rkt"
         (prefix-in simple: "parse/simpleParser.rkt")
         (prefix-in function: "parse/functionParser.rkt")
         (prefix-in class: "parse/classParser.rkt"))

(provide (all-defined-out))
  
;; Useful functions not in the scope of the interpreter project
;; Some depend on a modified copy of lex and the parsers,
;; and thus would not function properly in the environment used for grading

(define interpret-template
  (lambda (interpret-proc parse-proc)
    (lambda (input
             #:return [return  default-return]
             #:user-exn [user-exn  (default-user-exn)]
             #:throw [throw  (default-throw user-exn)]
             . args)
      (apply interpret-proc (parse-proc input) (append args (list return throw user-exn))))))


(define interpret-v1-str  (interpret-template interpret-parse-tree-v1 simple:parser-str))
(define interpret-v1-file (interpret-template interpret-parse-tree-v1 simple:parser))

(define interpret-v2-str  (interpret-template interpret-parse-tree-v2 function:parser-str))
(define interpret-v2-file (interpret-template interpret-parse-tree-v2 function:parser))

(define interpret-v3-str  (interpret-template interpret-parse-tree-v3 class:parser-str))
(define interpret-v3-file (interpret-template interpret-parse-tree-v3 class:parser))

;; str equivalent of the `interpret` function provided by `interpreter`
;; AKA latest version of interpret
(define interpret-str interpret-v3-str)
