#lang racket

(require "../util/map.rkt"
         "function.rkt")

(provide new-function-table
         (prefix-out fun-table:
                     (combine-out has-fun?
                                  all
                                  get-all
                                  get
                                  declare-fun)))

;;;; function table
;; a function table a map of function bindings
;; bindings { name : function }

(define new-function-table map:empty)

(define all
  (lambda (table)
    (map cdr table)))
; get all functions with this name in the table
(define get-all map:get-all)
;; gets the function with a matching signature
; false if absent
(define get
  (lambda (name arg-list table)
    (findf (lambda (f)
             (eq? (length arg-list)
                  (function:num-formal-params f)))
           (get-all name table))))

; get returns false if absent
(define has-fun?
  (lambda (name arg-list table)
    (not (eq? #f (get name arg-list table)))))


(define declare-fun
  (lambda (name params body scoper scope class table)
    (map:insert name
                (function:of #:name name
                             #:params params
                             #:body body
                             #:scoper scoper
                             #:scope scope
                             #:class class)
                table)))


(module+ test
  (require rackunit)
  (check-false (has-fun? 'a '() new-function-table))
  (check-eq? '() (get-all 'a new-function-table))

  
  (let* ([fname    'a]
         [fparams  '(a b & c)]
         [fargs    '(1 2 var)]
         [table    (declare-fun fname fparams null null null null new-function-table)])
    (check-true (has-fun? fname fargs table))
    (check-false (has-fun? fname '() table))
    (check-eq? (get fname fargs table) (first (get-all fname table)))))

