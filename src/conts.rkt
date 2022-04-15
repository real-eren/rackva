#lang racket/base

(require "util/map.rkt")

(provide (combine-out conts-of
                      w/suffix
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
    (map:from-interlaced-entries
     return-key   ret
     break-key    brk
     continue-key con
     next-key     nxt
     throw-key    thr)))

;;;; Helper function for adding a common 'and-then' effect to conts
(define identity (lambda (v) v))
(define w/suffix
  (lambda (conts
           #:state-fun [sfun identity]
           #:error-fun [efun identity]
           #:value-fun [vfun identity])
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

(define getter
  (lambda (key)
    (lambda (conts) (map:get key conts))))


(define return-key 'return)
(define return (getter return-key))

(define break-key 'break)
(define break (getter break-key))

(define continue-key 'continue)
(define continue (getter continue-key))

(define next-key 'next)
(define next (getter next-key))

(define throw-key 'throw)
(define throw (getter throw-key))
