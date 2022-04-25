#lang racket

(require "function.rkt")

(provide new-function-table
         (prefix-out fun-table:
                     (combine-out has?
                                  get-all
                                  get
                                  declare-fun)))

;;;; function table
;; a function table a map of function bindings
;; bindings { name : function }

(define new-function-table null)

; get all functions with this name in the table
(define get-all
  (lambda (name table)
    (filter (lambda (func)
              (eq? name (function:name func)))
            table)))
;; gets the function with a matching signature
; #F on miss
(define get
  (lambda (name arg-list table)
    (findf (lambda (f)
             (eq? (length (filter (lambda (a) (not (eq? '& a))) arg-list))
                  (function:num-formal-params f)))
           (get-all name table))))

; get returns false if absent
(define has?
  (lambda (name arg-list table)
    (not (false? (get name arg-list table)))))


(define declare-fun
  (lambda (name params body scoper scope class table)
    (cons (function:of #:name name
                       #:params params
                       #:body body
                       #:scoper scoper
                       #:scope scope
                       #:class class)
          table)))


(module+ test
  (require rackunit)
  (check-false (has? 'a '() new-function-table))
  (check-eq? '() (get-all 'a new-function-table))

  
  (let* ([fname    'a]
         [fparams  '(a b & c)]
         [fargs    '(1 2 var)]
         [table    (declare-fun fname fparams null null null null new-function-table)])
    (check-true (has? fname fargs table))
    (check-false (has? fname '() table))
    (check-eq? (get fname fargs table) (first (get-all fname table)))))

