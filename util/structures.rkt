#lang racket
(require "my-lens.rkt")

(provide (combine-out struc-1
                      struc-2
                      struc-3
                      struc-4
                      e1-lens
                      e2-lens
                      e3-lens
                      e4-lens))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; list-backed "structures"
;; currently O(n) access time
;; to use:
;;
;; (require "structures.rkt")
;; ;; constructor
;; (define my-type-with-k-elems struc-k)
;;
;; ;; constructor with named parameters
;; (define my-type
;;   (lambda (apple banana cherry)
;;     (struc-3 apple banana cherry)))
;;
;; ;; lens for each member
;; (define my-type-apple-lens e1-lens)
;; etc
;; ;; or just the getter/setter
;; (define my-type-apple (getter e1-lens))
;; (define my-type-set-apple (setter e1-lens))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define struc-1
  (lambda (e1)
    (list e1)))

(define struc-2
  (lambda (e1 e2)
    (list e1 e2)))

(define struc-3
  (lambda (e1 e2 e3)
    (list e1 e2 e3)))

(define struc-4
  (lambda (e1 e2 e3 e4)
    (list e1 e2 e3 e4)))

(define e1-lens (list-kth-elem-lens 1))
(define e2-lens (list-kth-elem-lens 2))
(define e3-lens (list-kth-elem-lens 3))
(define e4-lens (list-kth-elem-lens 4))
