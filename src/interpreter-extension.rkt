#lang racket

(require "interpreter.rkt"
         (prefix-in simple: "simpleParser.rkt")
         (prefix-in function: "functionParser.rkt")
         (prefix-in class: "classParser.rkt"))

(provide interpret-str
         interpret-v1-str
         interpret-v1-file
         interpret-v2-str
         interpret-v2-file
         interpret-v3-str
         interpret-v3-file)
  
;; Useful functions not in the scope of the interpreter project
;; Some depend on a modified copy of lex and the parsers,
;; and thus would not function properly in the environment used for grading


;; takes a string representing a program, interprets it
;; returns the result
; extra white space, including new lines, is ignored
; ex:
; (interpret-str "
; line1
; line2")

(define interpret-v1-str
  (lambda (str [return default-return] [throw default-throw])
    (interpret-parse-tree-v1 (simple:parser-str str)
                             return
                             throw)))

(define interpret-v1-file
  (lambda (filename [return default-return] [throw default-throw])
    (interpret-parse-tree-v1 (simple:parser filename)
                             return
                             throw)))


(define interpret-v2-str
  (lambda (str [return default-return] [throw default-throw])
    (interpret-parse-tree-v2 (function:parser-str str)
                             return
                             throw)))

(define interpret-v2-file
  (lambda (filename [return default-return] [throw default-throw])
    (interpret-parse-tree-v2 (function:parser filename)
                             return
                             throw)))

(define interpret-v3-str
  (lambda (str entry-point [return default-return] [throw default-throw])
    (interpret-parse-tree-v3 (class:parser-str str)
                             entry-point
                             return
                             throw)))

(define interpret-v3-file
  (lambda (filename entry-point [return default-return] [throw default-throw])
    (interpret-parse-tree-v3 (class:parser filename)
                             entry-point
                             return
                             throw)))

;; str equivalent of the `interpret` function provided by `interpreter`
;; AKA latest version of interpret
(define interpret-str interpret-v3-str)
