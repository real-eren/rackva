#lang racket

(require (combine-in "map-entry.rkt"))
(require (only-in "map-entry.rkt"
                  make-entry
                  entry-key
                  entry-value))

(provide (prefix-out map: lookup))
     ;                ((result-has-value
      ;               result-get-value
       ;              map-lookup-default
        ;             map-insert))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; MAP ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;; map primitives ;;;;;;;;;;
;; dependency on entry primitives

;; returns, if present, the entry in the map-list with the given key
(define lookup
  (lambda (key list-map entry-key entry-value)
    (cond
      [(null? list-map)                        (make-lookup-result #f '())]
      [(equal? key
               (entry-key (car list-map)))     (entry-value (car list-map))]
      [else                                    (lookup key (cdr list-map) entry-key entry-value)])))


;; lenses for return value of lookup w/out default value
(define result-has-value
  (lambda (lookup-result)
    (car lookup-result)))
; undefined if has-value is false
(define result-get-value
  (lambda (lookup-result) ; can check has-value for debugging purposes
    (cdr lookup-result)))
; constructs a result for a lookup
(define make-lookup-result
  (lambda (present value)
    (cons present value)))

;; returns value of binding 
(define map-lookup-default
  (lambda (key default list-map)
    (cond
      [(null? list-map)                         default]
      [(equal? key
               (entry-key (car list-map)))      (entry-value (car list-map))]
      [else                                     (map-lookup-default key (cdr list-map))])))
;; inserts the key-value pair into the map-list
;; does not check if an entry with the same key exists
(define map-insert
  (lambda (key value list-map)
    (cons (make-entry key value) list-map)))

;; removes the first entry with matching key
(define map-remove
  (lambda (key list-map)
    (cond
      [(null? list-map)                                 list-map]
      [(equal? key (entry-key (car list-map)))          (cdr list-map)]
      [else                                             (cons (car list-map)
                                                              (map-remove key (cdr list-map)))])))

;; removes every entry whose key matches 
(define map-remove-every
  (lambda (key list-map)
    (cond
      [(null? list-map)                                 list-map]
      [(equal? key (entry-key (car list-map)))          (map-remove key (cdr list-map))]
      [else                                             (cons (car list-map)
                                                              (map-remove key (cdr list-map)))])))

;; inserts the entry, removing any entries with matching keys
(define map-replace
  (lambda (key value list-map)
    (map-insert key value (map-remove-every key list-map))))




