#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; type implementation
;; types can be compared to
;;  determine if type 'a' accepts type 'b'
;;
;; this should leave room for type unions and sub-typing
;; ! not guaranteed to be associative


;; returns whether the first type
;; is considered to accept
;; the second type
;; ex: in Java, int accepts int
;; List accepts ArrayList
;; 
(define first-type-accepts-second-type
  (lambda (first-type second-type)
    #t)) ; to-do
