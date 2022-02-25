#lang racket/base

(require "util/map.rkt")

(provide (combine-out return
                      set-return
                      break
                      set-break
                      next
                      set-next
                      continue
                      set-continue))

;;;; Container of continuations used by the interpreter
;; these include return, next, break, continue

(define empty map-empty)

;; takes an initial conts and returns a new conts
;; with the changes applied
; ex: (conts-of oldconts #:next mynext #:return myreturn)
(define conts-of
  (lambda (conts
           #:return   [ret (return conts)]
           #:break    [brk (break conts)]
           #:continue [con (continue conts)]
           #:next     [nxt (next conts)])
    (map-from-interlaced-entry-list
     (list return-key   ret
           break-key    brk
           continue-key con
           next-key     nxt)
     map-empty)))

(define setter
  (lambda (key)
    (lambda (value conts) (map-put key value conts))))

(define getter
  (lambda (key)
    (lambda (conts) (map-get key conts))))


(define return-key 'return)
(define return (getter return-key))
(define set-return (setter return-key))

(define break-key 'break)
(define break (getter break-key))
(define set-break (setter break-key))

(define continue-key 'continue)
(define continue (getter continue-key))
(define set-continue (setter continue-key))

(define next-key 'next)
(define next (getter next-key))
(define set-next (setter next-key))
