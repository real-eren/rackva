#lang racket

(provide (prefix-out map-
                     (combine-out empty
                                  empty-custom
                                  empty?
                                  entry-list
                                  eq-fun
                                  contains?
                                  get
                                  get-default
                                  put
                                  put-if-absent
                                  remove
                                  from-interlaced-entry-list)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; A list-backed map ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; By default, uses `equal?` to compare
;; elements.
;; Using the ___ constructor,
;; you can also provide a   
;; comparator to be used.
;; Pair (key-comp-func and entry list)
;;
;; functions prefixed with _ take the entrylist of a map

;;;;;;;;;;;; map entry ;;;;;;;;;;;;;;;;;;;
(define entry-of cons)
(define entry-key car)
(define entry-value cdr)

;;;;;;;;;;;; map primitives ;;;;;;;;;;;;;;

;; takes an entry list and a eq-fun
;; and returns a map
(define construct 
  (lambda (entry-list eq-fun) ; do not replace with cons - wrong arg order
    (cons eq-fun entry-list)))

;; returns the key equality function used by the given map
(define eq-fun car)
;; returns a list of the entries in the given map
(define entry-list cdr)


;; creates an empty map
(define empty (construct '() equal?))
;; returns an empty map that will use your
;; given equality function to compare keys
;; your eq-fun should take two keys and
;; return whether they are equal
(define empty-custom
  (lambda (eq-fun)
    (construct '() eq-fun)))

;; returns whether the map is empty
(define empty?
  (lambda (map)
    (null? (entry-list map))))


;; returns the first entry from the given entry list
; could weaken to "some" or "any"
(define first car)
;; returns a list containing every entry except that returned by first
(define rest cdr)

;;;;;;;; map functions ;;;;;;;;

; returns whether an entry with the given key exists
(define contains?
  (lambda (key map)
    (_contains? key (entry-list map) (eq-fun map))))

(define _contains?
  (lambda (key entry-list eq-fun?)
    (ormap (lambda (entry) (eq-fun? key (entry-key entry))) entry-list)))

;; returns, if present, the value of the entry in the map-list with the given key
;; else null
(define get
  (lambda (key map)
    (get-default key null map)))

;; returns value of first encountered entry with key,
;; returns default if no entries found
(define get-default
  (lambda (key default map)
    (_get-default key
                  default
                  (entry-list map)
                  (eq-fun map))))

(define _get-default
  (lambda (key default entry-list eq-fun?)
    (cond
      [(null? entry-list)                                     default]
      [(eq-fun? key (entry-key (first entry-list)))           (entry-value (first entry-list))]
      [else                                                   (_get-default key default (rest entry-list) eq-fun?)])))


;; adds the key-value pair to the map
;; does not check if an entry with the same key exists
(define insert
  (lambda (key value map)
    (construct (_insert key value map)
               (eq-fun map))))

(define _insert
  (lambda (key value entry-list)
    (insert-entry (entry-of key value) entry-list)))

;; inserts the entry into the entry list
(define insert-entry cons)

;; removes the first entry with matching key
(define remove
  (lambda (key map)
    (construct (_remove key (entry-list map) (eq-fun map))
               (eq-fun map))))

(define _remove
  (lambda (key entry-list eq-fun?)
    (cond
      [(null? entry-list)                               entry-list]
      [(eq-fun? key (entry-key (first entry-list)))     (rest entry-list)]
      [else                                             (cons (first entry-list)
                                                              (_remove key (rest entry-list) eq-fun?))])))

;; inserts the entry, removing any previous entry with a matching key
(define put
  (lambda (key value map)
    (construct (_insert key
                        value
                        (_remove key
                                 (entry-list map)
                                 (eq-fun map)))
               (eq-fun map))))

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
  (lambda (lis map)
    (construct (_from-interlaced-entry-list lis (entry-list map))
               (eq-fun map))))

(define _from-interlaced-entry-list
  (lambda (lis entrylist)
    (if (null? lis)
        entrylist
        (_from-interlaced-entry-list (cdr (cdr lis))
                                     (_insert (first lis) (second lis) entrylist)))))


