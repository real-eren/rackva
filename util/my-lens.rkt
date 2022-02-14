#lang racket
(provide (combine-out make-lens
                      getter
                      setter
                      
                      lens-view
                      lens-set
                      lens-transform
                      
                      identity-lens
                      lens-compose
                      lens-thrush
                      
                      car-lens
                      cdr-lens
                      first-lens
                      second-lens
                      list-kth-elem-lens))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; A bare-bones implementation of the lens pattern ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mostly adheres to the API found at                    ;;
;; https://docs.racket-lang.org/lens/lens-reference.html ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; A lens, as implemented here, is a pair of a getter and a setter
;; foo-bar-lens = (foo-bar-getter . foo-bar-setter)
;; The setter is not a mutating function,
;; rather, it returns a copy with the changes applied
;; 
;; ex:
;; point-x-lens is a lens that takes a point as a 'target'
;;   and produces a 'view' of the x member
;;
;; (lens-view point point-x-lens) returns the x member of point
;; 
;; (lens-set point point-x-lens 2.1) produces a copy of point with the x member == 2.1
;; 
;; This module allows you to
;; * treat getter/setters as first-class values
;; * Compose them to access deeply nested members
;; * Re-use them across structures with identical shapes
;;


;; takes a getter and a setter
(define make-lens cons)
;; returns the getter of a lens
(define getter car)
;; returns the setter of a lens
(define setter cdr)

;; applies lenz's getter to target
(define lens-view
  (lambda (lenz target)
    ((getter lenz) target)))

;; applies lenz's setter to target with val
;; note: does NOT mutate target.
;; Returns a copy of target with val replacing the member viewed via lenz
(define lens-set
  (lambda (lenz target val)
    ((setter lenz) target val)))

;; transforms the view (via l) of t with mapping function f
;; get, transform, set
(define lens-transform
  (lambda (lenz target transform)
    (lens-set (setter lenz)
              target
              (transform (lens-view lenz target)))))

;; performs no destructuring
;; (combine2 lenz identity-lens) -> lenz
;; (combine2 identity-lens lenz) -> lenz
(define identity-lens
  (make-lens (lambda (target) target)
             (lambda (target val) val)))


;; first-lens accepts the view of second-lens
;; ex: cadr-lens = (combine2 car-lens cdr-lens)
;; views the car of the cdr
(define combine2
  (lambda (first-lens second-lens)
    (cond
      [(eq? second-lens identity-lens)      first-lens]
      [(eq? first-lens identity-lens)      second-lens]
      [else
       (make-lens (lambda (target)
                    (lens-view first-lens
                               (lens-view second-lens
                                          target)))
                  (lambda (target val)
                    (lens-set second-lens
                              target
                              (lens-set first-lens
                                        (lens-view second-lens
                                                   target)
                                        val))))])))

;; combines the lenses such that
;; 'eye' -> first lens -> ... -> last lens -> target
;; lens i consumes the view of lens i+1
;; ex: caddr-lens  car of cdr of cdr of target
;; (lens-compose '(car-lens cdr-lens cdr-lens))
(define lens-compose
  (lambda (lenses)
    ; (foldr combine2 identity-lens lenses)
    (cond
      [(null? lenses)           identity-lens]
      [(null? (cdr lenses))     (car lenses)]
      [else                     (combine2 (car lenses)
                                          (lens-compose (cdr lenses)))])))

;; combines the lenses such that
;; 'eye' -> last lens -> ... -> first lens -> target
;; opposite order of lens-compose
(define lens-thrush
  (lambda (lenses)
    (foldl combine2 identity-lens lenses)))

;;;;;;;; basic lenses ;;;;;;;;

(define car-lens
  (make-lens car
             (lambda (p v)
               (cons v (cdr p)))))

(define cdr-lens
  (make-lens cdr
             (lambda (p v)
               (cons (car p) v))))


(define first-lens car-lens)

(define second-lens (combine2 first-lens cdr-lens))

;; produces a lens that views the k-th element of a list
;; passing 1 returns car-lens, 2 cadr-lens, etc
(define list-kth-elem-lens
  (lambda (k)
    (if (= k 1)
        car-lens
        (combine2 (list-kth-elem-lens (- k 1)) cdr-lens))))
