#lang racket

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