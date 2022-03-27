#lang racket

(require (rename-in racket
                    (max greater)))

;;;; A red-black* BST
; keys are symbols or numbers
;  as low as log(n) insertion, access, replace, remove
; *balancing coming soon

;; node:  entry X tree-info
;; entry:  key . value
;; tree-info:   (color . (left . right))


;; a tree with no nodes
(define empty-tree null)
;; creates a tree with one node (root)
(define tree-of
  (lambda (k v)
    (node-of k v black)))

;; constructs a node with the given properties
(define node-of
  (lambda (k v c [l null] [r null])
    (cons (cons k v)
          (cons c (cons l r)))))

(define node-copy
  (lambda (node
           #:k [k (key node)]
           #:v [v (value node)]
           #:c [c (color node)]
           #:l [l (left node)]
           #:r [r (right node)])
    (node-of k v c l r)))

(define red #t)
(define black #f)

(define <?
  (lambda (k1 k2)
    (cond
      [(and (number? k1) (number? k2))    (< k1 k2)]
      [(and (symbol? k1) (symbol? k2))    (symbol<? k1 k2)]
      [else                               (error k1 k2 "invalid types")])))
(define = eq?)
(define != (negate =))

;; returns the key . value
(define entry car)

;; returns the key of a node
(define key (compose car entry))
;; returns the value held by a node
(define value (compose cdr entry))

;; returns whether the node is red or black. or 'removed
(define color cadr)
(define red? color)

;; returns the links of a node
; null if absent
(define left caddr)
(define right cdddr)


;;;;;;;; Access Functions ;;;;;;;;

;; returns the node with the greatest key in the tree. null if empty tree
(define max-node
  (lambda (root)
    (cond
      [(null? root)             null]
      [(null? (right root))     root]
      [else                     (max-node (right root))])))
;; returns the entry with the greatest key in the tree. null if empty tree
(define max
  (lambda (root)
    (if (null? root)
        null
        (entry (max-node root)))))

;; returns the node with the least key in the tree. null if empty tree
(define min-node
  (lambda (root)
    (cond
      [(null? root)             null]
      [(null? (left root))      root]
      [else                     (min-node (left root))])))
;; returns the entry with the least key in the tree. null if empty tree
(define min
  (lambda (root)
    (if (null? root)
        null
        (entry (min-node root)))))


;; returns the path (list of nodes) to the node with the given key
; an absent key's path ends with a null (element)
(define get-path
  (lambda (k root [lis null])
    (cond
      [(null? root)               (cons null lis)]
      [(<? k (key root))          (get-path k (left root) (cons root lis))]
      [(= k (key root))           (cons root lis)]
      [else                       (get-path k (right root) (cons root lis))])))

;; returns the node with the given key, or null
(define get-node
  (lambda (k root)
    (cond
      [(null? root)               null]
      [(<? k (key root))          (get-node k (left root))]
      [(= k (key root))           root]
      [else                       (get-node k (right root))])))

;; returns the value of the node with the given key
; error if not present
(define get
  (lambda (k root)
    (value (get-node k root))))

;; returns the value of the node with the given key if present, else default
(define get-default
  (lambda (k root default)
    (if (null? (get-node k root))
        default
        (value (get-node k root)))))


;; returns whether the tree contains the key
(define in?
  (lambda (k root)
    (not (null? (get-node k root)))))
(define contains? in?)


;;;;;;;; Update Functions ;;;;;;;;

(define put
  (lambda (k v root)
    (cond
      [(null? root)            (node-of k v red)]
      [(= k (key root))        (node-copy root #:v v)]
      [(<? k (key root))       (node-copy root #:l (put k v (left root)))]
      [else                    (node-copy root #:r (put k v (right root)))])))

(define remove
  (lambda (k root)
    (cond
      [(null? root)            root]
      [(<? k (key root))       (node-copy root #:l (remove k (left root)))]
      [(!= k (key root))       (node-copy root #:r (remove k (right root)))]
      ; key matches
      [(null? (left root))     (right root)]
      [(null? (right root))    (left root)]
      [else                    (let ([succ  (min-node (right root))])
                                 (node-copy root
                                            #:k (key succ)
                                            #:v (value succ)
                                            #:r (remove (key succ) (right root))))])))

;;;;;;;; Transformations ;;;;;;;;

(define from-interlaced-list
  (lambda (lis [tree empty-tree])
    (if (or (null? lis) (null? (cdr lis)))
        tree
        (from-interlaced-list (cddr lis) (put (car lis) (cadr lis) tree)))))
;; returns an ordered list of the entries in this tree
(define to-list
  (lambda (node)
    (if (null? node)
        null
        (append (to-list (left node))
                (list (entry node))
                (to-list (right node))))))
; L N R

;; returns the peak depth of the tree. empty tree has height 0, root is height 1
(define max-height
  (lambda (root)
    (if (null? root)
        0
        (+ 1 (greater (max-height (left root))
                      (max-height (right root)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)
  
  ; node not present in empty
  (check-false (in? 'key empty-tree))
  (check-equal? '() (to-list empty-tree))
  
  ;; tree size one
  ; node present after insert
  (check-true (in? 'key (put 'key 1 empty-tree)))
  ; node absent after insert+remove
  (check-false (in? 'key (remove 'key (put 'key 1 empty-tree))))
  ; returns correct value
  (check-equal? 2 (get 'key (put 'key 2 empty-tree)))
  ; replaces existing value
  (check-equal? 2 (get 'key (put 'key 2 (put 'key 1 empty-tree))))
  
  ;; tree size 2
  (define tree-2 (put 'b 22 (put 'a 11 empty-tree)))
  (check-true (and (in? 'b tree-2) (in? 'a tree-2)))
  (check-equal? 22 (get 'b tree-2))
  (check-equal? 11 (get 'a tree-2))
  (check-equal? (cons 'a 11) (min tree-2))
  (check-equal? (cons 'b 22) (max tree-2))
  
  (check-false (in? 'b (remove 'b tree-2)))
  (check-equal? '((a . 11) (b . 22))
                (to-list tree-2))
  
  ;; tree size 8
  (define tree-8 (put 'a 2
                      (put 'b 3
                           (put 'c 4
                                (put 'd 5
                                     (put 'e 6
                                          (put 'f 7
                                               (put 'g 8
                                                    (put 'h 9
                                                         empty-tree)))))))))

  (check-equal? '((a . 2) (b . 3) (c . 4) (d . 5) (e . 6) (f . 7) (g . 8) (h . 9))
                (to-list tree-8))
  (check-true (andmap (lambda (key) (in? key tree-8))
                      '(a b c d e f g h)))
  (check-equal? '(2 3 4 5 6 7 8 9)
                (map (lambda (k) (get k tree-8)) '(a b c d e f g h)))
  
  (check-equal? (cons 'a 2) (min tree-8))
  (check-equal? (cons 'h 9) (max tree-8))

  ;; tree size 4 with 4 removed nodes
  (define tree-4 (remove 'h (remove 'f (remove 'd (remove 'b tree-8)))))
  (check-true (andmap (lambda (key) (in? key tree-4))
                      '(a c e g)))
  (check-false (ormap (lambda (key) (in? key tree-4))
                      '(b d f h)))

  ;; tree size 7 
  (define tree-7 (put 'b 0 (put 'd 'e (put 'f null tree-4))))
  (check-true (andmap (lambda (key) (in? key tree-7))
                      '(a b c d e f g)))
  (check-false (in? 'h tree-7))

  
  ;; overwriting same element(s) multiple times
  (define tree-overwrite-1 (from-interlaced-list (list 'a 2.1
                                                       'b 3.1
                                                       'c 4.1
                                                       'd 5.1) tree-8))

  (check-equal? '(2.1 3.1 4.1 5.1 6 7 8 9)
                (map (lambda (key) (get key tree-overwrite-1))
                     '(a b c d e f g h)))

  (check-true (andmap (lambda (key) (in? key tree-overwrite-1))
                      '(a b c d e f g h)))

  (define tree-overwrite-2 (from-interlaced-list (list 'a 2.2
                                                       'b 3.2
                                                       'c 4.2
                                                       'd 5.2)
                                                 tree-overwrite-1))
  
  (check-equal? '(2.2 3.2 4.2 5.2 6 7 8 9)
                (map (lambda (key) (get key tree-overwrite-2))
                     '(a b c d e f g h)))

  (check-true (andmap (lambda (key) (in? key tree-overwrite-2))
                      '(a b c d e f g h)))

  (define tree-overwrite-3 (put 'a 2.3
                                (remove 'a
                                        (put 'b 3.3
                                             (remove 'b
                                                     (put 'c 4.3
                                                          (remove 'c
                                                                  (put 'd 5.3
                                                                       (remove 'd tree-8)))))))))
  
  (check-equal? '(2.3 3.3 4.3 5.3 6 7 8 9)
                (map (lambda (key) (get key tree-overwrite-3))
                     '(a b c d e f g h)))

  (check-true (andmap (lambda (key) (in? key tree-overwrite-1))
                      '(a b c d e f g h)))

  
  )
