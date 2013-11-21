#lang racket

(require "../Lab5/Lab5.rkt")
(require racket/stream)

(define writeln
  (lambda (ls)
    (write ls)
    (newline)))

; macros (MIT Scheme to Racket)
; cons-stream ==> stream-cons
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)
(define null-stream empty-stream)

(define accumulate
  (lambda (op base term ls)
    (cond ((null? ls)
           base)
          (else
           (op (term (car ls))
               (accumulate op base term (cdr ls)))))))

;test function
(define test-me
  (lambda (n)
    (formatter "test.txt" "test2.txt" n)))

;main function
(define formatter 
  (lambda (input-filename output-filename line-length)
    (stream->file output-filename
      (right-justify line-length
        (insert-newlines line-length
          (remove-extra-spaces
            (remove-newlines
              (file->stream input-filename))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define my-stream->list
  (lambda (stream n)
	  (cond ((stream-null? stream) '())
		((zero? n) '())
		(else (cons (stream-car stream)
			    (my-stream->list (stream-cdr stream) (- n 1)))))))

(define write-list
  (lambda (ls output-port)
    (cond ((null? ls) '())
          (else 
           (begin
             (write-char (car ls) output-port)
             (write-list (cdr ls) output-port))))))

(define file->stream 
  (lambda (filename)
    (let ((in-port (open-input-file filename)))
      (letrec
        ((build-input-stream
          (lambda ()
            (let ((ch (read-char in-port)))
              (if (eof-object? ch)
                  (begin
                    (close-input-port in-port)
                    (stream))
                  (stream-cons ch (build-input-stream)))))))
        (build-input-stream)))))

(define stream->file
  (lambda (filename str)
    (let ((out-port (open-output-file filename #:exists 'replace)))
      (letrec
          ((build-output-stream
            (lambda (str)
              (if (stream-empty? str)
                    (begin
                      (close-output-port out-port)
                      (stream))
                    (begin
                      (write-char (stream-first str) out-port)
                      (build-output-stream (stream-rest str)))))))
        (build-output-stream str)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define insert-newlines
  (lambda (line-length str)
    (letrec
      ((insert
        (lambda (str count)
	  (if (stream-empty? str)
	      str
	      (let ((n (count-chars-to-next-space str)))
	        (if (and (< count line-length) 
		         (<= (+ n count) line-length))
		    (stream-cons
		      (stream-first str)
		      (insert (stream-rest str) (+ count 1)))
		    (stream-cons
		      #\newline
		      (insert (trim-spaces str) 0))))))))
      (insert (trim-spaces str) 0))))

(define trim-spaces 
  (lambda (str)
    (cond ((stream-empty? str) (stream))
	  ((char=? (stream-first str) #\space)
	   (trim-spaces (stream-rest str)))
	  (else str))))

(define count-chars-to-next-space 
  (lambda (str)
    (letrec
      ((count-ahead
        (lambda (str count)
	  (cond ((stream-empty? str) count)
	        ((char=? (stream-first str) #\space) count)
	        (else (count-ahead (stream-rest str) (+ count 1)))))))
      (count-ahead str 0))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define remove-newlines
  (lambda (str)
    (cond ((stream-empty? str) (stream))
          ((char=? #\newline (stream-first str))
           (stream-cons #\space (remove-newlines (stream-rest str))))
          (else (stream-cons (stream-first str) (remove-newlines (stream-rest str)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stream-cadr
  (lambda (str)
    (if (stream-null? (stream-cdr str))
        #\null
        (stream-car (stream-cdr str)))))

(define remove-extra-spaces
  (lambda (str)
    (cond ((stream-null? str) '())
          ((char=? #\space (stream-car str))
           (if (char=? #\space (stream-cadr str))
               (remove-extra-spaces (stream-cdr str))
               (stream-cons (stream-car str)
                            (remove-extra-spaces (stream-cdr str)))))
          (else (stream-cons (stream-car str)
                             (remove-extra-spaces (stream-cdr str)))))))
               
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define spaces
  (lambda (count)
    (cond ((zero? count) '())
          (else (append (list #\space)
                        (spaces (- count 1)))))))

(define delete
  (lambda (ls n)
    (if (= n 0) 
        (cdr ls)
        (append (list (car ls)) (delete (cdr ls) (- n 1))))))

(define ls-last
  (lambda (list)
    (car (reverse list)))) 

(define delete-trailing-space
  (lambda (ls)
    (if (char=? #\space (ls-last ls))
        (delete ls (- (length ls) 1))
        ls)))
;;returns first line of a body of text
(define text-car
  (lambda (text)
    (letrec
        ((helper
          (lambda ()
            (cond ((stream-null? text) (stream)) 
                  ((char=? #\newline (stream-car text))
                           (stream))
                  (else (stream-cons (stream-car text)
                                     (text-car (stream-cdr text))))))))
      (stream->list (helper)))))

;;returns a body of text without the first line
(define text-cdr
  (lambda (text)
    (letrec
        ((helper
          (lambda (str)
            (if (stream-null? str) 
                (stream)
                (stream-cons (stream-car str)
                             (helper (stream-cdr str)))))))
      (cond ((char=? #\newline (stream-car text))
             (helper (stream-cdr text)))
            (else (text-cdr (stream-cdr text)))))))
            

(define count-line-chars
  (lambda (str)
    (let ((line (stream->list str)))
    (accumulate + 0 (lambda (x) 1) line))))

(define count-line-spaces
  (lambda (ls)
    (accumulate + 
                0 
                (lambda (x) 
                  (if (char=? #\space x)
                      1
                      0))
                ls)))

(define justify
  (lambda (ls spaces-needed gaps)
    (let ((q (quotient spaces-needed gaps))
          (r (modulo spaces-needed gaps)))
    (letrec
        ((helper
          (lambda (list num-spaces extra-spaces)
            (cond ((null? list) '())
                  ((zero? extra-spaces)
                   (if (char=? #\space (car list))
                       (append (spaces (+ 1 num-spaces)) (helper (cdr list) num-spaces extra-spaces))
                       (cons (car list) (helper (cdr list) num-spaces extra-spaces))))
                  (else (if (char=? #\space (car list))
                            (append (spaces (+ 2 num-spaces)) (helper (cdr list) num-spaces (- extra-spaces 1)))
                            (cons (car list) (helper (cdr list) num-spaces extra-spaces))))))))
      (helper ls q r)))))
          

(define right-justify
  (lambda (width str)
    (let ((current-line (delete-trailing-space (text-car str))))
      (let ((spaces-needed (- width (count-line-chars current-line)))
            (gaps (count-line-spaces current-line)))
        (writeln (list "gaps = " gaps))
        (cond ((stream-null? str) (stream))
              (else (stream-cons (justify current-line spaces-needed gaps)
                                 (right-justify width (text-cdr str)))))))))      
      