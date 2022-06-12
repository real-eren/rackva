; If you are not using racket, comment these two lines
#lang racket/base
(provide start-lex
         end-lex
         peek-next-symbol
         get-next-symbol
         unget-next-symbol
         parsing-error)

;;===============================================================
;; The Lexical Analyzer
;;
;; CSDS 345
;=======================================

; get and unget the next symbol from the lexical analyzer
; A 1 symbol buffer is used so the last read symbol can be pushed
; back to the analyzer
(define last-symbol-saved? #f)   ; is there a symbol buffered?
(define saved-symbol #f)        ; the symbol buffer
(define line-number 0)

(define in-port '())

;; A 1 character buffer is used so the last read character can be
;; pushed back
(define saved-last-char #f)    ; is the last character buffered?
(define last-read-char #f)     ; the character buffer

(define (reset-global-vars)
  (set! last-symbol-saved? #F)
  (set! saved-symbol #F)
  (set! line-number 0)
  (set! saved-last-char #F)
  (set! last-read-char #F)
  (set! in-port '()))

; get the next symbol without consuming it
(define (peek-next-symbol)
  (when (not last-symbol-saved?)
    (begin
      (set! saved-symbol (lex))
      (set! last-symbol-saved? #t)))
  saved-symbol)

; get and consume the next symbol to be processed
(define get-next-symbol
  (lambda ()
    (if last-symbol-saved?
        (begin
          (set! last-symbol-saved? #f)
          saved-symbol)
        (begin
          (set! saved-symbol (lex))
          (set! last-symbol-saved? #f)
          saved-symbol))))

; mark the last symbol sent as unread so that it can be read again
;
(define unget-next-symbol
  (lambda ()
    (begin
      (set! last-symbol-saved? #t))))

; read and consume the next character from the port

(define readchar 
  (lambda (port)
    (if saved-last-char
        (begin
          (set! saved-last-char #f)
          last-read-char)
        (let ([char  (read-char port)])
          (when (eqv? #\newline char)
            (set! line-number (+ 1 line-number)))
          char))))

; read, without consuming, the next character
(define peekchar
  (lambda (port)
    (when (not saved-last-char)
      (set! saved-last-char #T)
      (set! last-read-char (read-char port))
      (when (eqv? #\newline last-read-char)
        (set! line-number (+ 1 line-number))))
    last-read-char))
          

; unread the last character from the port so it can be read again

(define unreadchar
  (lambda (lastchar port)
    (begin (set! last-read-char lastchar)
           (set! saved-last-char #t))))

; set the input port

(define start-lex
  (lambda (port)
    (reset-global-vars)
    (set! in-port port)))


; close the input port, in case it's a fileport

(define end-lex
  (lambda ()
    (close-input-port in-port)
    (reset-global-vars)))

(define (parsing-error msg)
  (raise-user-error 'parser (format "near line ~a:\n~a"
                                    line-number msg)))

; the current list of reserved words and operator characters

(define reserved-word-list '(if else return while break continue class extends new throw catch finally try static var true false function))
(define reserved-operator-list '(#\= #\< #\> #\! #\+ #\* #\/ #\- #\% #\& #\| #\!))

; return a lexeme with the next read symbol

(define return-id-lex 
  (lambda (id)
    (if (memq id reserved-word-list)
        (if (or (eq? id 'false) (eq? id 'true))
            (cons 'BOOLEAN id)
            (cons id '()))
        (cons 'ID id))))

(define (return-num-lex value) (cons 'NUMBER value))

(define (return-symbol-lex symbol) (cons 'BINARY-OP symbol))

(define (return-left-paren-lex) (cons 'LEFTPAREN '()))

(define (return-right-paren-lex) (cons 'RIGHTPAREN '()))

(define (return-assign-lex symbol) (cons 'ASSIGN symbol))

(define (return-null-lex symbol) (cons 'UNKNOWN symbol))

(define (return-semicolon-lex) (cons 'SEMICOLON '()))

(define (return-leftbrace-lex) (cons 'LEFTBRACE '()))

(define (return-rightbrace-lex) (cons 'RIGHTBRACE '()))

(define (return-comma-lex) (cons 'COMMA '()))

(define (return-eof-lex) (cons 'EOF '()))

(define (return-period-lex) (cons 'BINARY-OP 'dot))

; The lexical analyer.  Keep reading characters until the next symbol is found.
; then return that symbol

(define lex
  (lambda ()
    (let ((nextchar (readchar in-port)))
      (cond
        ((eof-object? nextchar) (return-eof-lex))
        ((char-whitespace? nextchar) (lex))
        ((char-alphabetic? nextchar) (return-id-lex (string->symbol (id-lex in-port (make-string 1 nextchar)))))
        ((char-numeric? nextchar) (return-num-lex (num-lex in-port (addtointeger 0 nextchar))))
        ((char=? #\* nextchar) (if (char=? (peekchar in-port) #\/)
                                   (parsing-error "")
                                   (return-symbol-lex (string->symbol (symbol-lex in-port (make-string 1 nextchar))))))
        ; comments. if /, peek next * -> */ or / -> /n
        ((char=? #\/ nextchar) (let ([nextnextchar (readchar in-port)])
                                 (case nextnextchar
                                   [(#\/)    (line-comment-lex) (lex)]
                                   [(#\*)    (block-comment-lex) (lex)]
                                   [else     (unreadchar nextnextchar in-port)
                                             (return-symbol-lex (string->symbol (symbol-lex in-port (make-string 1 nextchar))))])))
        ((memq nextchar reserved-operator-list) (return-symbol-lex (string->symbol (symbol-lex in-port (make-string 1 nextchar)))))
        ((char=? #\( nextchar) (return-left-paren-lex))
        ((char=? #\) nextchar) (return-right-paren-lex))
        ((char=? #\; nextchar) (return-semicolon-lex))
        ((char=? #\{ nextchar) (return-leftbrace-lex))
        ((char=? #\} nextchar) (return-rightbrace-lex))
        ((char=? #\, nextchar) (return-comma-lex))
        ((char=? #\. nextchar) (return-period-lex))
        (else (return-null-lex nextchar))))))


(define id-lex
  (lambda (fport idstring)
    (let ((nextchar (readchar fport)))
      (if (or (char-alphabetic? nextchar) (char-numeric? nextchar) (char=? #\_ nextchar))
          (id-lex fport (string-append idstring (make-string 1 nextchar)))
          (begin (unreadchar nextchar fport)
                 idstring)))))

(define addtointeger
  (lambda (val nextdigit)
    (+ (* val 10) (- (char->integer nextdigit) (char->integer #\0)))))

(define num-lex
  (lambda (fport value)
    (let ((nextchar (readchar fport)))
      (if (char-numeric? nextchar)
          (num-lex fport (addtointeger value nextchar))
          (begin (unreadchar nextchar fport)
                 value)))))

(define symbol-lex
  (lambda (fport idstring)
    (let ((nextchar (readchar fport)))
      (if (memq nextchar reserved-operator-list)
          (symbol-lex fport (string-append idstring (make-string 1 nextchar)))
          (begin (unreadchar nextchar fport)
                 idstring)))))

; returns nothing. consumes characters until newline or eof
(define line-comment-lex
  (lambda ()
    (let ([nextchar (readchar in-port)])
      (if (or (eqv? #\newline nextchar)
              (eof-object? nextchar))
          (unreadchar nextchar in-port)
          (line-comment-lex)))))

; returns nothing. consumes characters until */
(define block-comment-lex
  (lambda ([saw-star? #F])
    (let ([nextchar (readchar in-port)])
      (cond
        [(eof-object? nextchar)   (parsing-error "unclosed comment")]
        [(and saw-star? (eqv? #\/ nextchar))]
        [(eqv? #\* nextchar)      (block-comment-lex #T)]
        [else                     (block-comment-lex #F)]))))
