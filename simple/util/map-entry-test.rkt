#lang racket

(require rackunit
         (only-in "map-entry.rkt"
                  entry:of
                  entry:key
                  entry:value))

(check-equal? (entry:key (entry:of 'a 'b))       'a)
(check-equal? (entry:value (entry:of 'a 'b))     'b)