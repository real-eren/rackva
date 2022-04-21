#lang racket

(require rackunit)

(provide (all-defined-out))

;;;; Utilities for creating unit tests

;; take an arbitrary ID and create a well-formatted message
;; used to create error messages for unit tests
(define format-test-id
  (lambda (id)
    (if (eq? id #f)
        #f
        (string-append "test ID: " (if (string? id)
                                       id
                                       (format "~a" id))))))

;; takes a interpret function and return a function
;; that takes an expected value, a program, an optional Test-ID,
;; optional additional args to pass to the interpreter
;; and asserts that the result of running intepret == expected value
;; the interpret function given determines the type of input accepted

(define make-tester
  (lambda (interpret)
    (lambda (expected ; expected result
             program ; passed as program to interpret
             #:args [args '()] ; list of additional arguments to pass to interpret
             #:id [test-id #f] ; Test ID used if testcase fails
             )
      (check-equal? (apply interpret program args) expected (format-test-id test-id)))))


;; like make-tester except returns a function for asserting that a user-error is thrown
;; returned function takes following arguments:
; program, optional kw:
; id (test ID)
; catch (T/F, False to display error message)
; arglist

(define make-error-tester
  (lambda (interpret)
    (lambda (program ; program passed to interpret
             #:args [args '()] ; list of additional arguments to pass to interpret
             #:id [test-id #f] ; Test ID used if testcase fails
             #:catch [suppress #t] ; #T to run normally, #F to display error message
             )
      (if suppress
          (check-exn exn:fail:user? (lambda () (apply interpret program args)) (format-test-id test-id))
          (check-eq? 'error (apply interpret program args) #:id test-id)))))
