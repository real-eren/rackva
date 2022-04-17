#lang racket/base

(require "util/map.rkt")

(provide (combine-out conts-of
                      w/preproc
                      
                      return
                      break
                      continue
                      next
                      throw))

;;;; Container of continuations used by the interpreter
;; these include return, next, break, continue, throw, catch, finally

(define empty-conts map:empty)

;; takes an initial conts and returns a new conts
;; with the changes applied
; ex: (conts-of oldconts #:next mynext #:return myreturn)
(define conts-of
  (lambda ([conts empty-conts]
           #:return   [ret (return conts)]
           #:break    [brk (break conts)]
           #:continue [con (continue conts)]
           #:next     [nxt (next conts)]
           #:throw    [thr (throw conts)])
    (map:of
     return-key   ret
     break-key    brk
     continue-key con
     next-key     nxt
     throw-key    thr)))

;;;; Helper function for applying a common mapping function to the state, error and value params
(define identity (lambda (v) v))
;; returns conts that behave like the given conts but w/ preprocessing on e, s, and v
(define w/preproc
  (lambda (conts
           #:map-state [sfun identity]
           #:map-error [efun identity]
           #:map-value [vfun identity])
    (conts-of
     #:return  (lambda (v s)
                 ((return conts) (vfun v) (sfun s)))
     #:break   (lambda (s)
                 ((break conts) (sfun s)))
     #:continue (lambda (s)
                  ((continue conts) (sfun s)))
     #:next     (lambda (s)
                  ((next conts) (sfun s)))
     #:throw    (lambda (e s)
                  ((throw conts) (efun e) (sfun s))))))


(define return-key 'return)
(define return (map:getter return-key))

(define break-key 'break)
(define break (map:getter break-key))

(define continue-key 'continue)
(define continue (map:getter continue-key))

(define next-key 'next)
(define next (map:getter next-key))

(define throw-key 'throw)
(define throw (map:getter throw-key))
