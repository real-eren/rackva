#lang racket

(require rackunit
         "map.rkt")

;; newly constructed maps are empty
(check-true (map-empty? map-empty))
(check-true (map-empty? (map-empty-custom eq?)))

(define map-3 (map-put 'a 1 (map-put 'b 2 (map-put 'c 3 map-empty))))
(define map-3i (map-from-interlaced-entry-list '(a 1
                                                 b 2
                                                 c 3)
                                               map-empty))

; may have to replace with unordered equality
(check-true (equal? (map-entry-list map-3)
                    (reverse (map-entry-list map-3i))))

;; contains finds most recent
(check-true (map-contains? 'a map-3))
;; middle entered
(check-true (map-contains? 'b map-3))
;; first entered
(check-true (map-contains? 'c map-3))
;; only 3 elements
(check-eq? 3 (length (map-entry-list map-3)))
;; not present
(check-false (map-contains? 'not-in-map map-3))

(check-true (map-contains? 'a (map-remove 'not-a-key map-3)))
(check-true (map-contains? 'a (map-remove 'b map-3)))


;;;; updating existing map
;; entry gone after removing
(check-false (map-contains? 'a (map-remove 'a map-3)))

(check-true (map-contains? 'r (map-put 'r 123 map-3)))
(check-eq? 4 (length (map-entry-list (map-put 'r 123 map-3))))

(check-true (map-contains? 'a (map-put 'a 123 map-3)))

(check-true (let ([new-v 123]
                  [key   'a])
              (let ([new-map (map-put key new-v map-3)])
                (and (map-contains? key new-map)
                     (eq? (map-get key new-map) new-v)))))

;; removing each element results in an empty map
(check-true (let ([new-map (map-remove 'c
                                       (map-remove 'b
                                                   (map-remove 'a map-3)))])
              (map-empty? new-map)))

;; remove-every removes multiple
(check-false (map-contains? 'b (map-remove 'b (map-put 'b 0 map-3))))
;; remove-every doesn't remove other
(check-true (map-contains? 'a (map-remove 'b (map-put 'b 0 map-3))))


;; put-if-absent behaves as claimed
(check-true (map-contains? 'a map-3))
(check-true (map-contains? 'a (map-put-if-absent 'a 999 map-3)))
(check-eq? (map-get 'a map-3) (map-get 'a (map-put-if-absent 'a 999 map-3)))

(check-false (map-contains? 'z map-3))
(check-true (map-contains? 'z (map-put-if-absent 'z 999 map-3)))
(check-eq? 999 (map-get 'z (map-put-if-absent 'z 999 map-3)))

