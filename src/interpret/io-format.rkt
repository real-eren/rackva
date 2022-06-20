#lang racket/base
(provide format-val-for-output)
(require "state/instance.rkt"
         racket/format
         racket/string)

;;;;
;; this module contains functions for the formatting of values output by the intepreter

;; takes a value and modifies it for output
; int as number
; boolean as symbol (matching source code)
; instance as JSON string (WIP)
(define format-val-for-output
  (lambda (value [indentation 0])
    (cond
      [(eq? #t value)         'true]
      [(eq? #f value)         'false]
      [(number? value)        value]
      [(is-instance? value)   (instance->json-string value indentation)]
      [else                   (error "returned an unsupported type: " value)])))


(define instance->json-string
  (lambda (i [indentation 0])
    (define indent (make-string (* 4 (+ 1 indentation)) #\space))
    (define (f vtable)
      (define entry-indent (make-string (* 4 (+ 3 indentation)) #\space))
      (string-append indent "    {"
                     (string-join (map (lambda (entry)
                                         (string-append entry-indent
                                                        "\"" (symbol->string (car entry)) "\""
                                                        ": "
                                                        (~a (format-val-for-output (unbox (cdr entry)) (+ 3 indentation)))))
                                       vtable)
                                  ",\n"
                                  #:before-first (if (null? vtable) "" "\n")
                                  #:after-last (if (null? vtable) "" (string-append "\n    " indent)))
                     "}"))
    (string-append "{\n"
                   indent
                   "\"class\": \""
                   (symbol->string (instance:class i))
                   "\",\n"
                   indent
                   "\"fields\": [\n"
                   (string-join (map f (instance:fields i))
                                ",\n")
                   "\n"
                   indent
                   "]\n"
                   (make-string (* 4 indentation) #\space)
                   "}")))

