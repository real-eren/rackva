#lang racket

(provide (prefix-out map:
                     (combine-out empty
                                  empty?
                                  contains?
                                  get
                                  get-default
                                  put
                                  put-if-absent
                                  remove
                                  
                                  from-interlaced-entries
                                  from-interlaced-entry-list

                                  getter
                                  setter)))
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
        (replace key value map)
        (insert key value map))))

;; replaces the first entry with a matching key to have the given value
; assumes key is in map
(define replace
  (lambda (key value map)
    (if (equal? key (entry-key (first map)))
        (insert-entry (entry-of key value) (rest map))
        (insert-entry (first map) (replace key value (rest map))))))

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

;; `Factories` for key-specific getters and setters
(define getter
  (lambda (key)
    (lambda (map)
      (get key map))))
(define setter
  (lambda (key)
    (lambda (value map)
      (put key value map))))


;;;; Unit Tests
(module+ test
  (require rackunit)

  ;; newly constructed maps are empty
  (check-true (empty? empty))

  (define map-3 (put 'a 1 (put 'b 2 (put 'c 3 empty))))
  (define map-3i (from-interlaced-entry-list '(a 1
                                               b 2
                                               c 3)
                                             empty))

  ; may have to replace with unordered equality
  (check-true (equal? map-3
                      (reverse map-3i)))

  ;; contains finds most recent
  (check-true (contains? 'a map-3))
  ;; middle entered
  (check-true (contains? 'b map-3))
  ;; first entered
  (check-true (contains? 'c map-3))
  ;; only 3 elements
  (check-eq? 3 (length map-3))
  ;; not present
  (check-false (contains? 'not-in-map map-3))

  (check-true (contains? 'a (remove 'not-a-key map-3)))
  (check-true (contains? 'a (remove 'b map-3)))


  ;;;; updating existing map
  ;; entry gone after removing
  (check-false (contains? 'a (remove 'a map-3)))

  (check-true (contains? 'r (put 'r 123 map-3)))
  (check-eq? 4 (length (put 'r 123 map-3)))

  (check-true (contains? 'a (put 'a 123 map-3)))

  (check-true (let ([new-v 123]
                    [key   'a])
                (let ([new-map (put key new-v map-3)])
                  (and (contains? key new-map)
                       (eq? (get key new-map) new-v)))))

  ;; removing each element results in an empty map
  (check-true (let ([new-map (remove 'c
                                     (remove 'b
                                             (remove 'a map-3)))])
                (empty? new-map)))

  ;; remove-every removes multiple
  (check-false (contains? 'b (remove 'b (put 'b 0 map-3))))
  ;; remove-every doesn't remove other
  (check-true (contains? 'a (remove 'b (put 'b 0 map-3))))


  ;; put-if-absent behaves as claimed
  (check-true (contains? 'a map-3))
  (check-true (contains? 'a (put-if-absent 'a 999 map-3)))
  (check-eq? (get 'a map-3) (get 'a (put-if-absent 'a 999 map-3)))

  (check-false (contains? 'z map-3))
  (check-true (contains? 'z (put-if-absent 'z 999 map-3)))
  (check-eq? 999 (get 'z (put-if-absent 'z 999 map-3)))
  )
