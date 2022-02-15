#lang racket

(require "context.rkt")

(define interpret
  (lambda (statement-list)
    (print (context-current-value (Mcontext (cdr statement-list)
                                            (Mcontext (car statement-list)
                                                      (default-context)))))))

;; to-do
(define Mcontext
  (lambda (expression context)
    (cond
      [(context-return? context)        context]
      )))
