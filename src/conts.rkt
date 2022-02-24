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
                      
(define setter
  (lambda (key)
    (lambda (value map) (map-put key value map))))

(define getter
  (lambda (key)
    (lambda (map) (map-get key map))))


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
