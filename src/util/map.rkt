#lang racket

(require (only-in "map-entry.rkt"
                  entry:of
                  entry:key
                  entry:value))

(provide (prefix-out map:
                     (combine-out empty
                                  empty?
                                  get
                                  result:has-value?
                                  result:get-value
                                  get-default
                                  insert
                                  remove
                                  remove-every
                                  replace
                                  entry:key
                                  entry:value)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; A list-backed map ;;;;;;;;;;;;;
;; aka an association-list            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;; map primitives ;;;;;;;;;;;;;;
;; dependency: entry primitives

;; creates an empty map
(define empty '())

;; returns whether the map is empty
(define empty? null?)

;; returns some entry from the map
; could weaken to "some" or "any"
(define first car)

;; returns a map containing every entry except that returned by first
(define rest cdr)


;;;;;;;; map functions ;;;;;;;;

;; returns, if present, the entry in the map-list with the given key
(define get
  (lambda (key map)
    (cond
      [(empty? map)                               (result #f '())]
      [(equal? key (entry:key (first map)))       (result #t (entry:value (first map)))]
      [else                                       (get key (rest map))])))


;; lenses for return value of lookup w/out default value
(define result:has-value?
  (lambda (result)
    (car result)))
; returns the value from lookup. undefined if has-value is false
(define result:get-value
  (lambda (result) ; can add check has-value for debugging purposes
    (cdr result)))
; constructs a result for a lookup
(define result
  (lambda (present? value)
    (cons present? value)))


;; returns value of first encountered entry with key,
;; returns default if no entries found
(define get-default
  (lambda (key default map)
    (cond
      [(empty? map)                                   default]
      [(equal? key (entry:key (first map)))           (entry:value (first map))]
      [else                                           (get-default key default (rest map))])))


;; inserts the key-value pair into the map
;; does not check if an entry with the same key exists
(define insert
  (lambda (key value map)
    (insert-entry (entry:of key value) map)))

;; inserts the entry into the map
(define insert-entry
  (lambda (entry map)
    (cons entry map)))

;; removes the first entry with matching key
(define remove
  (lambda (key map)
    (cond
      [(empty? map)                                    map]
      [(equal? key (entry:key (first map)))            (rest map)]
      [else                                            (insert-entry (first map)
                                                               (remove key (rest map)))])))

;; removes every entry whose key matches 
(define remove-every
  (lambda (key map)
    (cond
      [(empty? map)                                   map]
      [(equal? key (entry:key (first map)))           (remove-every key (rest map))]
      [else                                           (insert-entry (first map)
                                                              (remove-every key (rest map)))])))

;; inserts the entry, removing any entries with matching keys
(define replace
  (lambda (key value map)
    (insert key value (remove-every key map))))

