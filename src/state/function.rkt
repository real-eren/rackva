#lang racket/base

(require "../util/map.rkt"
         racket/list
         racket/string)

(provide function->string
         (prefix-out function: (except-out (all-defined-out)
                                           function->string)))

;;;; function property keys
(define $name   'fun-name)
(define name (map:getter $name))
;; list of formal parameters
(define $params 'formal-params)
(define params (map:getter $params))
;; statement list that forms the body of the function
(define $body   'body)
(define body (map:getter $body))
;; state mapping function that filters the layers that should be visible during execution
(define $scoper 'scoper)
(define scoper (map:getter $scoper))
; what class this function is defined in, if applicable.
; null or absent if type == free
(define $class  'class)
(define class (map:getter $class))
;; what kind of function is this
(define $scope   'scope)
(define scope (map:getter $scope))
; constants for Static | Instance | Free | Abstract | Init
(define scope:static   'static)
(define scope:instance 'instance)
(define scope:free     'free)
(define scope:abstract 'abstract)
(define scope:init     'init)
(define scope:constructor 'constructor)

(define scope-checker
  (lambda (scp)
    (lambda (f)
      (equal? (scope f) scp))))

(define static? (scope-checker scope:static))
(define constructor? (scope-checker scope:constructor))
(define init? (scope-checker scope:init))
(define instance? (scope-checker scope:instance))
(define abstract? (scope-checker scope:abstract))


(define of
  (lambda (#:name name
           #:params params
           #:body body
           #:scoper scoper
           #:scope scope
           #:class class)
    (map:of
     $name    name
     $params  params
     $body    body
     $scoper  scoper
     $scope   scope
     $class   class)))

;; returns the number of formal parameters in a function
(define num-formal-params
  (lambda (closure)
    ; ignore ampersands, they mark the following symbol as by-reference
    (length (filter (lambda (p) (not (eq? '& p))) (map:get $params closure)))))

;; Given a valid function, produces a nicely formatted string
(define function->string
  (lambda (fun)
    (format "~a~a"
            (if (class fun)
                (format "~a::" (class fun))
                "")
            (formatted-signature (name fun) (params fun)))))

(define formatted-signature
  (lambda (fun-name params)
    (format "~a(~a)"
            fun-name
            (string-join (params->strs params) ", "))))

(define params->strs
  (lambda (params)
    (cond
      [(null? params)              null]
      [(eq? '& (first params))     (cons (format "&~a" (second params))
                                         (params->strs (cddr params)))]
      [else                        (cons (format "~a" (first params))
                                         (params->strs (rest params)))])))
