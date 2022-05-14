#lang racket
(require "util/map.rkt")

(provide (prefix-out ue: (combine-out raise-exn
                                      break-outside-loop
                                      continue-outside-loop)))
;;;; User Errors

; lambda (e cl)

; e is an error instance
; produced by calling functions here
; with parameters from call-site

; error instance
; map {
;   type : 'type
;   field1 : x
;   field2 : y
; }
; this way, easy to check type but can also
; check the error's fields in unit-tests

(define raise-exn
  (lambda (exn cl)
    ; generate source from cl (foldl)
    ; format e to message
    ; raise user error
    (raise-user-error (format "~a~n~a"
                              (exn->string exn)
                              (context-list->string cl)))))

(define exn:of
  (lambda (type fields values)
    (foldl map:put
             (map:of 'type type)
             fields
             values)))

(define fff
  (lambda (type . fields)
    (lambda values
      (exn:of type fields values))))

(define f1 (fff 'f 'a 'b 'c))

;; exception types
(define type:break-outside-loop 'break-outside-loop)
(define type:continue-outside-loop 'continue-outside-loop)

;; exception constructors

(define break-outside-loop (exn:of type:break-outside-loop '() '()))
(define continue-outside-loop (exn:of type:continue-outside-loop '() '()))


;; exception to string
(define exn->string
  (lambda (exn)
    ((exn-type->format-proc (map:get 'type exn)) exn)))

; returns a proc that takes an exn of type `type` and produces a formatted string
(define exn-type->format-proc
  (lambda (type)
    (map:get type exn-formatter-table)))

(define exn-formatter-table
  (map:of
   type:break-outside-loop  (λ (exn) "break statement outside of loop")
   type:continue-outside-loop  (λ (exn) "continue statement outside of loop")))

;; for break and continue outside of loop:
; by default they're absent
; if absent, conts returns (λ (s) (user-exn (ue:break-outside-loop) (list (context:break))))

(define ue-cl-add
  (lambda (context ue)
    (lambda (e cl)
      (ue e (cons context cl)))))

(define context-list->string
  (lambda (cl)
    (format "~a" cl)))

; cl - context list
; built by intepreter as it enters Mstates and Mvalues
; in order of execution - head is base
;


