#lang racket/base

(require rackunit 
  "interpreter.rkt")

(define test-file
  (lambda (expected file)
    (check-equal? (interpret file) expected)))

(define test-str
  (lambda (expected str)
    (check-equal? (interpret-str str) expected)))

; ; Number Tests
(test-file 150  "1.txt")
(test-file  -4  "2.txt")
(test-file  10  "3.txt")
(test-file  16  "4.txt")
(test-file 220  "5.txt")
(test-file   5  "6.txt")
(test-file   6  "7.txt")
(test-file  10  "8.txt")
(test-file   5  "9.txt")
(test-file -39 "10.txt")

; ; Error Tests
; ; NOTE THAT THIS DOES NOT TEST THAT YOU THROW THE CORRECT ERRORS
(check-exn exn:fail? (lambda () (interpret "11.txt")))
(check-exn exn:fail? (lambda () (interpret "12.txt")))
(check-exn exn:fail? (lambda () (interpret "13.txt")))
(check-exn exn:fail? (lambda () (interpret "14.txt")))

; ; Boolean, If, While Tests
(test-file 'true   "15.txt")
(test-file 100     "16.txt")
(test-file 'false  "17.txt")
(test-file 'true   "18.txt")
(test-file 128     "19.txt")
(test-file  12     "20.txt")


; ; Advanced Tests
; ; Advanced Test Cases
(test-file   30  "21.txt")
(test-file   11  "22.txt")
(test-file 1106  "23.txt")
(test-file   12  "24.txt")
(test-file   16  "25.txt")
(test-file   72  "26.txt")
(test-file   21  "27.txt")
(test-file  164  "28.txt")

