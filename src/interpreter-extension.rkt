#lang racket

(require "interpreter.rkt"
         "lex.rkt"
         "simpleParser.rkt")

(provide interpret-str)
  
;; Useful functions not in the scope of the interpreter project
;; These depend on a modified copy of lex and simpleParser,
;; and thus would not function properly in the environment used for grading


;; takes a string representing a program, intreprets it
;; returns the result
; extra white space, including new lines, is ignored
; ex:
; (interpret-str "
; var x = 1;
; return x;")

(define interpret-str
  (lambda (str)
    (interpret-parse-tree (parser-str str))))