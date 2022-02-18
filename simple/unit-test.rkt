#lang racket/base

; ; CSDS:345 Mark Wang
; ; README
; ; This unit test file assumes 3 things
; ; 1. That the module file's name is assignment1.rkt
; ; 2. That the module is inside the same directory that contains the test folder
; ; 3. That there is a line in the code that resembles the following:
; ;    (provide interp)
; ; 4. The interp function will take in a syntax tree, and an empty list indicating the state and return a value
; ; 5. A value constitutes either a number, 'true, 'false

; ; The module will print nothing if all assertions are correct, but will print a file error otherwise.

; ; Feel free to change the filename inside the double quotes to the name of your module file.
(require rackunit 
  "interpreter.rkt"
  "state.rkt"
  "simpleParser.rkt")

(define interp
  (lambda (stmt-list mm)
    (extract-result (Mstate-stmt-list stmt-list new-state))))

; ; Number Tests
(check-equal? (interp (parser "1.txt") '()) 150)
(check-equal? (interp (parser "2.txt") '()) -4)
(check-equal? (interp (parser "3.txt") '()) 10)
(check-equal? (interp (parser "4.txt") '()) 16)
(check-equal? (interp (parser "5.txt") '()) 220)
(check-equal? (interp (parser "6.txt") '()) 5)
(check-equal? (interp (parser "7.txt") '()) 6)
(check-equal? (interp (parser "8.txt") '()) 10)
(check-equal? (interp (parser "9.txt") '()) 5)
(check-equal? (interp (parser "10.txt") '()) -39)

; ; Error Tests
; ; NOTE THAT THIS DOES NOT TEST THAT YOU THROW THE CORRECT ERRORS
(check-exn exn:fail? (lambda () (interp (parser "11.txt") '())))
(check-exn exn:fail? (lambda () (interp (parser "12.txt") '())))
(check-exn exn:fail? (lambda () (interp (parser "13.txt") '())))
(check-exn exn:fail? (lambda () (interp (parser "14.txt") '())))

; ; Boolean, If, While Tests
(check-equal? (interp (parser "15.txt") '()) 'true)
(check-equal? (interp (parser "16.txt") '()) 100)
(check-equal? (interp (parser "17.txt") '()) 'false)
(check-equal? (interp (parser "18.txt") '()) 'true)
(check-equal? (interp (parser "19.txt") '()) 128)
(check-equal? (interp (parser "20.txt") '()) 12)


; ; Advanced Tests
; ; Advanced Test Cases
(check-equal? (interp (parser "21.txt") '()) 30)
(check-equal? (interp (parser "22.txt") '()) 11)
(check-equal? (interp (parser "23.txt") '()) 1106)
(check-equal? (interp (parser "24.txt") '()) 12)
(check-equal? (interp (parser "25.txt") '()) 16)
(check-equal? (interp (parser "26.txt") '()) 72)
(check-equal? (interp (parser "27.txt") '()) 21)
(check-equal? (interp (parser "28.txt") '()) 164)