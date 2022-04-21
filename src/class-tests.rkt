#lang racket
(require rackunit
         "interpreter-extension.rkt"
         "util/testing.rkt")

; white space / newlines do not affect the parser,
; but are included for readability


(define test-file
  (lambda (expected entry-point file #:id [test-id #f])
    (check-equal? (interpret-v3-file file entry-point) expected (format-test-id test-id))))

(define test-str
  (lambda (expected entry-point str #:id [test-id #f])
    (check-equal? (interpret-v3-str str entry-point) expected (format-test-id test-id))))

(define error-file
  (lambda (entry-point file #:id [test-id #f] #:catch [suppress #t])
    (if suppress
        (check-exn exn:fail:user? (lambda () (interpret-v3-file file entry-point)) (format-test-id test-id))
        (test-file 'error file #:id test-id))))

(define error-str
  (lambda (entry-point str #:id [test-id #f] #:catch [suppress #t])
    (if suppress
        (check-exn exn:fail:user? (lambda () (interpret-v3-str str entry-point)) (format-test-id test-id))
        (test-str 'error str #:id test-id))))

(test-str #:id "single class, no fields, only main method"
          10
          "A" "
class A {
  static function main() {
    return 10;
  }
}")


