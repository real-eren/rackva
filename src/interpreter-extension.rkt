#lang racket

(require "interpreter.rkt"
         "lex.rkt"
         (prefix-in simple: "simpleParser.rkt")
         (prefix-in function: "functionParser.rkt"))

(provide interpret-str
         simple-interpret-str
         simple-interpret-file
         function-interpret-str
         function-interpret-file)
  
;; Useful functions not in the scope of the interpreter project
;; Some depend on a modified copy of lex and simpleParser,
;; and thus would not function properly in the environment used for grading


;; takes a string representing a program, interprets it
;; returns the result
; extra white space, including new lines, is ignored
; ex:
; (interpret-str "
; var x = 1;
; return x;")

(define simple-interpret-str
  (lambda (str)
    (simple-interpret-parse-tree (simple:parser-str str))))

(define simple-interpret-file
  (lambda (filename)
    (simple-interpret-parse-tree (simple:parser filename))))


(define function-interpret-str
  (lambda (str)
    (interpret-parse-tree (function:parser-str str))))

(define function-interpret-file
  (lambda (filename)
    (interpret-parse-tree (function:parser filename))))

(define interpret-str function-interpret-str)
