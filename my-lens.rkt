#lang racket

(define make-lens cons)
(define getter car)
(define setter cdr)


(define lens-view
  (lambda (l t)
    ((getter l) t)))
    
(define lens-set
  (lambda (l t val)
    ((setter l) t val)))

(define lens-transform
  (lambda (l t f)
    (lens-set (setter l)
              t
              (f (lens-view l t)))))


(define identity-lens
  (make-lens (lambda (t) t)
             (lambda (t val) val)))


; apply outer lens to result of inner lens
(define combine2
  (lambda (outer-lens inner-lens)
    (cond
      [(eq? outer-lens identity-lens)      inner-lens]
      [(eq? inner-lens identity-lens)      outer-lens]
      [else
       (make-lens (lambda (t)
                    (lens-view outer-lens
                               (lens-view inner-lens
                                          t)))
                  (lambda (t val)
                    (lens-set outer-lens
                              (lens-set t
                                        val))))])))

(define combine
  (lambda (lenses)
    (cond
      [(null? lenses)         identity-lens]
      [(null? (cdr lenses)    (car lenses))]
      [else                   (combine2 (car lenses) (combine (cdr lenses)))])))



(define car-lens
  (make-lens car
             (lambda (p c)
               (cons c (cdr p)))))

(define cdr-lens
  (make-lens cdr
             (lambda (p c)
               (cons (car p) c))))
