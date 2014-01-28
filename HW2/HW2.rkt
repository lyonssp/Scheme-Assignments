#lang racket

(require racket/stream)

;Stream Constructor
;(define stream
;  (lambda args
;    (cond ((null? args) empty-stream)
;          (else stream-cons (car args)
;                (stream (cdr args))))))

; macros (MIT Scheme to Racket)
; cons-stream ==> stream-cons
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)
(define null-stream empty-stream)

;test function
(define test-me
  (lambda (n)
    (formatter "hollow.txt" "out.txt" n)))

;main function
(define formatter 
 (lambda (input-filename output-filename line-length)
  (stream->file output-filename
   (fix-lines line-length 
    (right-justify line-length
     (insert-newlines line-length
       (remove-extra-spaces
         (remove-newlines
          (file->stream input-filename)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;
;;Accumulate on stream
(define str-accumulate
  (lambda (op base term str)
    (cond ((stream-null? str)
           base)
          (else
           (op (term (stream-car str))
               (str-accumulate op base term (stream-cdr str)))))))

;;Return stream's first n elements as a list
(define my-stream->list
  (lambda (stream n)
	  (cond ((stream-null? stream) '())
		((zero? n) '())
		(else (cons (stream-car stream)
			    (my-stream->list (stream-cdr stream) (- n 1)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;I/O FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(define str-length
  (lambda (str)
    (str-accumulate
     +
     0
     (lambda (x) 1)
     str)))

(define spaces
  (lambda (count)
    (cond ((zero? count) (stream))
          (else (stream-cons #\space
                        (spaces (- count 1)))))))

(define stream-reverse
  (lambda (str)
    (cond ((stream-null? str) (stream))
          (else (stream-append (stream-reverse (stream-cdr str))
                               (stream (stream-car str)))))))
    
    
(define str-delete
  (lambda (str index)
    (let ((count 0))
      (letrec 
          ((helper
            (lambda (strm count)
              (cond ((stream-null? strm) (stream))
                    ((equal? count index)
                     (helper (stream-cdr strm) (+ count 1)))
                    (else (stream-cons (stream-car strm)
                                       (helper (stream-cdr strm) (+ count 1))))))))
        (helper str 0)))))

(define line-last
  (lambda (str)
    (stream-car (stream-reverse str))))

(define delete-trailing-space
  (lambda (str)
    (if (char=? #\space (line-last str))
        (str-delete str (- (str-length str) 1))
        str)))

;;returns first line of a body of text
(define text-car
  (lambda (text)
    (cond ((stream-null? text) (stream))
          ((char=? #\newline (stream-car text))
           (stream))
          (else (stream-cons (stream-car text)
                             (text-car (stream-cdr text)))))))

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
      (cond ((stream-null? text) (stream))
            ((char=? #\newline (stream-car text))
             (helper (stream-cdr text)))
            (else (text-cdr (stream-cdr text)))))))

(define count-line-chars
  (lambda (str)
      (str-accumulate + 0 (lambda (x) 1) str)))

(define count-line-spaces
  (lambda (str)
    (str-accumulate + 
                0
                (lambda (x)
                  (if (char=? #\space x)
                      1
                      0))
                str)))

(define justify
  (lambda (line spaces-needed gaps)
    (let ((q (quotient spaces-needed gaps))
          (r (remainder spaces-needed gaps)))
    (letrec
        ((helper
          (lambda (stream num-spaces extra-spaces)
            (cond ((stream-null? stream) empty-stream)
                  ((zero? extra-spaces)
                   (if (char=? #\space (stream-car stream))
                       (stream-append (spaces (+ 1 num-spaces)) (helper (stream-cdr stream) num-spaces extra-spaces)) ;;Append existing space + number of spaces to distribute
                       (stream-cons (stream-car stream) (helper (stream-cdr stream) num-spaces extra-spaces))))
                  (else (if (char=? #\space (stream-car stream))
                            (stream-append (spaces (+ 2 num-spaces)) (helper (stream-cdr stream) num-spaces (- extra-spaces 1))) ;;Append existing space  + one available extra space + number of spaces to distribute
                            (stream-cons (stream-car stream) (helper (stream-cdr stream) num-spaces extra-spaces))))))))
      (helper line q r)))))
          
(define right-justify
  (lambda (width str)
    (if (not (stream-null? str))
        (let ((current-line (delete-trailing-space (text-car str))))
          (let ((spaces-needed (- width (count-line-chars current-line)))
                (gaps (count-line-spaces current-line)))
            (stream-append (justify current-line spaces-needed gaps)
                                  (right-justify width (text-cdr str)))))
        (stream))))

(define fix-lines
  (lambda (width str)
      (letrec 
          ((helper
            (lambda (stream width index)
              (cond ((stream-null? stream) empty-stream)
                    ((equal? (remainder index (+ width 1)) 0)
                     (stream-cons #\newline (helper stream
                                                    width
                                                    (+ index 1))))
                    (else (stream-cons (stream-car stream) 
                                       (helper (stream-cdr stream) width (+ index 1))))))))
        (helper str width 1))))