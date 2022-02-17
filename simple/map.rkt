#lang racket

(provide (prefix-out map-
                     (combine-out empty
                                  empty?
                                  contains?
                                  get
                                  result:has-value?
                                  result:get-value
                                  get-default
                                  insert
                                  remove
                                  remove-every
                                  replace
                                  from-interlaced-entry-list)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; A list-backed map ;;;;;;;;;;;;;
;; aka an association-list            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;; map entry ;;;;;;;;;;;;;;;;;;;
(define entry-of cons)
(define entry-key car)
(define entry-value cdr)

;;;;;;;;;;;; map primitives ;;;;;;;;;;;;;;

;; creates an empty map
(define empty '())

;; returns whether the map is empty
(define empty? null?)

;; returns some entry from the map
; could weaken to "some" or "any"
(define first-entry car)

;; returns a map containing every entry except that returned by first
(define rest cdr)


;;;;;;;; map functions ;;;;;;;;

; returns whether an entry with the given key exists
(define contains?
  (lambda (key map)
    (result:has-value? (get key map))))

;; returns, if present, the entry in the map-list with the given key
(define get
  (lambda (key map)
    (cond
      [(empty? map)                                     (result #f '())]
      [(equal? key (entry-key (first-entry map)))       (result #t (entry-value (first-entry map)))]
      [else                                             (get key (rest map))])))


(define result:has-value? car)
; returns the value from lookup. undefined if has-value is false
(define result:get-value cdr) ; can add has-value? check for debugging
; constructs a result for a lookup
(define result cons)


;; returns value of first encountered entry with key,
;; returns default if no entries found
(define get-default
  (lambda (key default map)
    (cond
      [(empty? map)                                   default]
      [(equal? key (entry-key (first-entry map)))           (entry-value (first-entry map))]
      [else                                           (get-default key default (rest map))])))


;; inserts the key-value pair into the map
;; does not check if an entry with the same key exists
(define insert
  (lambda (key value map)
    (insert-entry (entry-of key value) map)))

;; inserts the entry into the map
(define insert-entry cons)

;; removes the first entry with matching key
(define remove
  (lambda (key map)
    (cond
      [(empty? map)                                    map]
      [(equal? key (entry-key (first-entry map)))            (rest map)]
      [else                                            (insert-entry (first-entry map)
                                                                     (remove key (rest map)))])))

;; removes every entry whose key matches 
(define remove-every
  (lambda (key map)
    (cond
      [(empty? map)                                         map]
      [(equal? key (entry-key (first-entry map)))           (remove-every key (rest map))]
      [else                                                 (insert-entry (first-entry map)
                                                                          (remove-every key (rest map)))])))

;; inserts the entry, removing any entries with matching keys
(define replace
  (lambda (key value map)
    (insert key value (remove-every key map))))



;; treats the first and second elems as key and value
;; returns a map with the entries
(define from-interlaced-entry-list
  (lambda (lis map)
    (if (null? lis)
        map
        (from-interlaced-entry-list (cdr (cdr lis))
                                    (insert (first lis) (second lis) map)))))


