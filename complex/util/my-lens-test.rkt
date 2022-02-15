#lang racket
(require rackunit
         "my-lens.rkt")

;;;; unit tests for the more complex functions in my-lens
;; such as compose, thrush and transform

(check-equal? (lens-transform first-lens
                              '(1 1)
                              (lambda (i) (* 2 i)))
              '(2 1))

(check-equal? (lens-set (lens-compose (list second-lens first-lens))
                        '((1 a) (2 b) (3 c))
                        'aaa)
              '((1 aaa) (2 b) (3 c)))

(check-equal? (lens-set (lens-compose (list first-lens second-lens))
                        '((1 a) (2 b) (3 c))
                        200)
              '((1 a) (200 b) (3 c)))

(check-equal? (lens-set (lens-thrush (list second-lens first-lens))
                        '((1 a) (2 b) (3 c))
                        200)
              '((1 a) (200 b) (3 c)))

(check-equal? (lens-set (lens-compose (list car-lens cdr-lens cdr-lens))
                        '(((000 001) (010 011)) (100 101) 110 111)
                        'xyz)
              '(((000 001) (010 011)) (100 101) xyz 111))

(check-equal? (lens-set (lens-thrush (list cdr-lens cdr-lens car-lens))
                        '(((000 001) (010 011)) (100 101) 110 111)
                        'xyz)
              '(((000 001) (010 011)) (100 101) xyz 111))

