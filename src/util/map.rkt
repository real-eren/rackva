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

                                  get*
                                  put*
                                  in*?
                                  update*
                                  
                                  of
                                  of-list

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

; returns whether an entry with the given key exists in the map
(define contains?
  (lambda (key map)
    (ormap (lambda (entry)
             (and (pair? entry)
                  (equal? key (entry-key entry))))
           map)))

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

;;;; deep accessor functions
;; keys are applied left-to-right
;; invalid queries w/ get return null
;; syntax : (get* map key1 key2 key3 ...)
;;          (put* val map key1 key2 key3 ...)
;;          (update* update-fun map key1 key2 key3)
(define get*
  (lambda (map . keys)
    (if (null? keys)
        map
        (apply get* (get (car keys) map) (cdr keys)))))

;; if path DNE, creates it
(define put*
  (lambda (value map key . keys)
    (if (null? keys)
        (put key value map)
        (put key (apply put* value (get key map) keys) map))))

;; does the map contain this sequence of keys
(define in*?
  (lambda (map key . keys)
    (cond
      [(not (list? map))   #f]
      [(null? keys)        (contains? key map)]
      [else                (apply in*? (get key map) keys)])))

;; map an existing entry to a new value with a given function
;; map is unchanged if key absent
(define update*
  (lambda (updater map key . keys)
    (if (apply in*? map key keys)
        (apply put* (updater (apply get* map key keys)) map key keys)
        map)))

;; takes an initial map and adds the given list to it
;; does not check for uniqueness
;; treats the first and second elems as key and value
;; returns a map with the entries
(define of-list
  (lambda (lis [map empty])
    (if (null? lis)
        map
        (of-list (cdr (cdr lis))
                 (insert (first lis) (second lis) map)))))
(define of
  (lambda lis
    (of-list lis)))

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
  (define map-3i (of 'a 1
                     'b 2
                     'c 3))

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

  ;;;; deep accessor functions
  (define c-map (of 'c1 0
                    'c2 12
                    'c3 232))
  (define d-map (of 'd1 c-map
                    'd2 '()
                    'd3 7))
  (define e-map (of 'e1 5
                    'e2 d-map
                    'e3 7))
  (define f-map (of 'f1 5
                    'f2 6
                    'f3 e-map))
  (check-eq? 232 (get* f-map 'f3 'e2 'd1 'c3))
  (check-eq? 0 (get* c-map 'c1))
  (check-eq? '() (get* e-map 'e2 'd2))
  (check-eq? c-map (get* f-map 'f3 'e2 'd1))
  (check-eq? null (get* f-map 'f3 'e2 'd1 'x)) ; last key invalid
  (check-eq? null (get* f-map 'x 'e2 'd1)) ; first key invalid
  (check-eq? null (get* f-map 'x 'x 'x)) ; multiple invalid keys

  (check-true (in*? f-map 'f1))
  (check-true (in*? f-map 'f3 'e3))
  (check-true (in*? f-map 'f3 'e2 'd3))
  (check-true (in*? f-map 'f3 'e2 'd1 'c3))
  (check-true (in*? f-map 'f3 'e2 'd2))

  (check-false (in*? '() 'x))
  (check-false (in*? '(1 2 3) 'x))
  (check-false (in*? f-map 'x))
  (check-false (in*? f-map 'f3 'e1 'x))
  (check-false (in*? f-map 'f3 'x))
  (check-false (in*? f-map 'f3 'e2 'x))
  (check-false (in*? f-map 'f3 'e2 'd1 'x))
  (check-false (in*? f-map 'x 'x 'x))

  (define put-get
    (lambda (val map . keys)
      (apply get* (apply put* val f-map keys) keys)))
  (check-equal? 'new-value (put-get 'new-value f-map 'f3 'e2' 'd1 'c3))
  (check-equal? 'new-value (put-get 'new-value f-map 'f3 'e2' 'd1))
  (check-equal? 'new-value (put-get 'new-value f-map 'f3 'e2' 'x 'x 'x))

  ; don't modify map if key absent
  (check-equal? f-map (update* (λ (v) 0) f-map  'x))
  (check-equal? f-map (update* (λ (v) 0) f-map  'f1 'x))
  (check-equal? f-map (update* (λ (v) 0) f-map  'f3 'x))

  (define f-map1 (update* (lambda (v) 'new-value) f-map 'f2))
  (define f-map2 (update* - f-map 'f3 'e3))
  (define f-map3 (update* (λ (v) (* 2 v)) f-map 'f3 'e2 'd3))

  (check-eq? 'new-value (get* f-map1 'f2)) ; updated entry
  ; should be untouched
  (check-eq? e-map (get* f-map1 'f3))
  (check-eq? 5 (get* f-map1 'f1))
  
  (check-eq? -7 (get* f-map2 'f3 'e3)) ; updated entry
  ; should be untouched
  (check-eq? 5 (get* f-map2 'f1))
  (check-eq? 6 (get* f-map2 'f2))
  (check-eq? d-map (get* f-map2 'f3 'e2))
  (check-eq? 5 (get* f-map2 'f3 'e1))
  
  (check-eq? 14 (get* f-map3 'f3 'e2 'd3)) ;update entry
  ; should be untouched
  (check-eq? 5 (get* f-map3 'f1))
  (check-eq? 6 (get* f-map3 'f2))
  (check-eq? 5 (get* f-map3 'f3 'e1))
  (check-eq? 7 (get* f-map3 'f3 'e3))
  (check-eq? c-map (get* f-map3 'f3 'e2 'd1))
  (check-eq? '() (get* f-map3 'f3 'e2 'd2))
  )
