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
  (lambda (str)
    (interpret-parse-tree-v1 (simple:parser-str str)
                             default-return
                             default-throw)))

(define interpret-v1-file
  (lambda (filename)
    (interpret-parse-tree-v1 (simple:parser filename)
                             default-return
                             default-throw)))


(define interpret-v2-str
  (lambda (str)
    (interpret-parse-tree-v2 (function:parser-str str)
                             default-return
                             default-throw)))

(define interpret-v2-file
  (lambda (filename)
    (interpret-parse-tree-v2 (function:parser filename)
                             default-return
                             default-throw)))

(define interpret-v3-str
  (lambda (str entry-point)
    (interpret-parse-tree-v3 (class:parser-str str)
                             entry-point
                             default-return
                             default-throw)))

(define interpret-v3-file
  (lambda (filename entry-point)
    (interpret-parse-tree-v3 (class:parser filename)
                             entry-point
                             default-return
                             default-throw)))

;; str equivalent of the `interpret` function provided by `interpreter`
;; AKA latest version of interpret
(define interpret-str interpret-v2-str)
