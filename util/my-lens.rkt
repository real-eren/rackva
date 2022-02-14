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

;; takes a getter and a setter
(define make-lens cons)
;; returns the getter of a lens
(define getter car)
;; returns the setter of a lens
(define setter cdr)

;; applies l's getter to t
(define lens-view
  (lambda (l t)
    ((getter l) t)))

;; applies l's setter to t with val
;; note: does NOT mutate t.
;; Returns a copy of t with val replacing the view via l
(define lens-set
  (lambda (l t val)
    ((setter l) t val)))

;; transforms the view (via l) of t with mapping function f
(define lens-transform
  (lambda (l t f)
    (lens-set (setter l)
              t
              (f (lens-view l t)))))

;; performs no destructuring
;; (combine2 l identity-lens) -> l
;; (combine2 identity-lens l) -> l
(define identity-lens
  (make-lens (lambda (t) t)
             (lambda (t val) val)))


;; first-lens accepts the view of second-lens
;; ex: second-lens = (combine2 car-lens cdr-lens)
;; views the car of the cdr
(define combine2
  (lambda (first-lens second-lens)
    (cond
      [(eq? second-lens identity-lens)      first-lens]
      [(eq? first-lens identity-lens)      second-lens]
      [else
       (make-lens (lambda (t)
                    (lens-view first-lens
                               (lens-view second-lens
                                          t)))
                  (lambda (t val)
                    (lens-set second-lens
                              t
                              (lens-set first-lens
                                        (lens-view second-lens
                                                   t)
                                        val))))])))

;; combines the lenses such that
;; 'eye' -> first lens -> ... -> last lens -> target
;; lens i consumes the view of lens i+1
;; ex: lens into 3-dimensional matrix for elem at x=1,y=2,z=3
;; where each dimension is a row. x.y.z
;; (lens-compose '(third-lens second-lens first-lens))
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
;; lens i consumes the view of lens i-1
;; opposite order of lens-compose
(define lens-thrush
  (lambda (lenses)
    (foldl combine2 identity-lens lenses)))

;;;;;;;; basic lenses ;;;;;;;;

(define car-lens
  (make-lens car
             (lambda (p c)
               (cons c (cdr p)))))

(define cdr-lens
  (make-lens cdr
             (lambda (p c)
               (cons (car p) c))))


(define first-lens car-lens)

(define second-lens (combine2 first-lens cdr-lens))

;; produces a lens that views the k-th element of a list
;; passing 1 returns car-lens, 2 cadr-lens, etc
(define list-kth-elem-lens
  (lambda (k)
    (if (= k 1)
        car-lens
        (combine2 (list-kth-elem-lens (- k 1)) cdr-lens))))

