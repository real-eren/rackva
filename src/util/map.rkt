#lang racket

(provide (prefix-out map-
                     (combine-out empty
                                  empty?
                                  contains?
                                  get
                                  get-default
                                  put
                                  put-if-absent
                                  remove
                                  from-interlaced-entries
                                  from-interlaced-entry-list)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; A list-backed map ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;; map entry ;;;;;;;;;;;;;;;;;;;
(define entry-of cons)
(define entry-key car)
(define entry-value cdr)

;;;;;;;;;;;; map primitives ;;;;;;;;;;;;;;

;; an empty map
(define empty null)

;; returns whether the map is empty
(define empty? null?)


;;;;;;;; map functions ;;;;;;;;

; returns whether an entry with the given key exists
(define contains?
  (lambda (key map)
    (ormap (lambda (entry) (equal? key (entry-key entry))) map)))

;; returns, if present, the value of the entry in the map-list with the given key
;; else null
(define get
  (lambda (key map)
    (get-default key null map)))

;; returns value of first encountered entry with key,
;; returns default if no entries found
(define get-default
  (lambda (key default map)
    (cond
      [(empty? map)                               default]
      [(equal? key (entry-key (first map)))       (entry-value (first map))]
      [else                                       (get-default key default (rest map))])))


;; inserts the entry into the entry list
(define insert-entry cons)

;; adds the key-value pair to the map
;; does not check if an entry with the same key exists
(define insert
  (lambda (key value map)
    (insert-entry (entry-of key value) map)))

;; removes the first entry with matching key
(define remove
  (lambda (key map)
    (cond
      [(empty? map)                             map]
      [(equal? key (entry-key (first map)))     (rest map)]
      [else                                     (cons (first map)
                                                      (remove key (rest map)))])))

;; inserts the entry, removing any previous entry with a matching key
(define put
  (lambda (key value map)
    (if (contains? key map) ; calling remove when absent produces O(n) garbage
        (update key value map)
        (insert key value map))))

;; replaces the first entry with a matching key to have the given value
; assumes key is in map
(define update
  (lambda (key value map)
    (if (equal? key (entry-key (first map)))
        (insert-entry (entry-of key value) (rest map))
        (insert-entry (first map) (update key value (rest map))))))

;; inserts the entry iff there is not already an entry with the key
(define put-if-absent
  (lambda (key value map)
    (if (contains? key map)
        map
        (put key value map))))


;; takes an initial map and adds the given list to it
;; does not check for uniqueness
;; treats the first and second elems as key and value
;; returns a map with the entries
(define from-interlaced-entry-list
  (lambda (lis [map empty])
    (if (null? lis)
        map
        (from-interlaced-entry-list (cdr (cdr lis))
                                    (insert (first lis) (second lis) map)))))
(define from-interlaced-entries
  (lambda lis
    (from-interlaced-entry-list lis)))


