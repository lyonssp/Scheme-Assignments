#lang racket


; file->list
; reads a file of characters into a list
;
(define file->list
  (lambda (filename) 
    (let ((input-port (open-input-file filename))) 
      (letrec 
	((build-input-list 
	   (lambda () 
	     (let ((current-char (read-char input-port))) 
	       (if (eof-object? current-char) 
		 (begin (close-input-port input-port) 
			'()) 
		 (cons current-char (build-input-list))))))) 
	(build-input-list)))))

; list->file
; writes a list of characters to a file
;
(define list->file
  (lambda (filename ls) 
    (let ((output-port (open-output-file filename)))
      (letrec
          ((helper
            (lambda (ls)
              (cond ((null? ls)
                     (close-output-port output-port))
                    (else (write-char (car ls) output-port)
                          (helper (cdr ls)))))))
        (helper ls)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Streams (infinite lists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/stream)

; macros (MIT Scheme to Racket)
; cons-stream ==> stream-cons
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)
(define null-stream empty-stream)


; defining a stream (infinite list) of natural numbers
; uses "lazy" evaluation
(define naturals
  (let helper ((n 0))
       (stream-cons n
		    (helper (+ n 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mutators for stream and list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; my-stream->list
; constructs a list of the first n items from a stream
;
(define my-stream->list
  (lambda (stream n)
	  (cond ((stream-null? stream) '())
		((zero? n) '())
		(else (cons (stream-car stream)
			    (my-stream->list (stream-cdr stream) (- n 1)))))))

; list->stream
; constructs a stream from a list
;
(define list->stream
  (lambda (list)
	  (cond ((null? list) (stream))
		(else (stream-cons (car list)
				   (list->stream (cdr list)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; other functions on streams
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; adding two streams
(define stream-add
  (lambda (stream1 stream2)
	  (cond ((or (stream-null? stream1) (stream-null? stream2)) (stream))
		(else (stream-cons (+ (stream-car stream1) (stream-car stream2))
				   (stream-add (stream-cdr stream1) (stream-cdr stream2)))))))

; map function on streams
(define my-stream-map
  (lambda (op stream)
    (stream-cons (op (stream-car stream))
                 (my-stream-map op (stream-cdr stream)))))

; filter function on streams
(define my-stream-filter
  (lambda (test? stream)
    (cond ((test? (stream-car stream))
           (stream-cons (stream-car stream)
                        (my-stream-filter test? (stream-cdr stream))))
          (else (my-stream-filter test? (stream-cdr stream))))))
           
    
; defining a stream of fibonacci numbers
;  FIB(n) = FIB(n-1) + FIB(n-2) if n > 1
;  FIB(1) = 1
;  FIB(0) = 0
(define fibonaccis
  (let helper ((m 0) (n 1))
    (stream-cons (+ m n)
                 (helper n (+ m n)))))

; defining a stream of (positive) rational numbers
; a "rational" number is a pair (a . b) where a and b are positive integers
; note: the stream should not contain any repeats
; example:
;  (1 . 1) (1 . 2) (2 . 1) (1 . 3) (3 . 1) (1 . 4) (2 . 3) (3 . 2) (4 . 1) (1 . 5) ... 
(define rationals
  (let helper ((p 1) (q 1))
    (if (equal? (gcd p q) 1)
        (stream-cons (cons p q)
                     (cond ((equal? p 1)
                            (helper (+ p 1) q))
                           (else (helper (- p 1) (+ q 1)))))
        (helper (- p 1) (+ q 1)))))
           
                 
                 
  


;;;;;;;;;;;
; test jig
;;;;;;;;;;;

(define test-me
  (lambda (n)
    (list (my-stream->list rationals n) 
          (my-stream->list fibonaccis n)
    	  (my-stream->list (my-stream-map sqr naturals) n)
          (my-stream->list (my-stream-filter even? fibonaccis) n))))
