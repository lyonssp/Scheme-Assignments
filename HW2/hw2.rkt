#lang racket

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
  (lambda (filename stream)
    (let ((out (open-output-file filename #:exists 'replace)))
      (letrec
          ((build-output-file
            (lambda (stream)
              (cond ((stream-empty? stream) (close-output-port out))
                    (else
              (let ((ch (stream-first stream)))
                (write-char ch out)
                (build-output-file (stream-rest stream))))))))
        (build-output-file stream)))))

(define formatter 
  (lambda (input-filename output-filename line-length)
    (stream->file output-filename
      (right-justify line-length
        (insert-newlines line-length
          (remove-extra-spaces
            (remove-newlines
              (file->stream input-filename))))))))

(define add-spaces
    (lambda (width string)
    (letrec
      ((looper
        (lambda (lst req-spaces)
	  (cond ((= (length lst) 1) lst)
                ((= req-spaces 0) lst)
                ((and (not(eq? (car lst) #\space)) 
                      (eq? (cadr lst) #\space)) (set! req-spaces (- req-spaces 1)) 
                                                (append (list(car lst)) '(#\space) (looper (cdr lst) req-spaces)))
                (else (cons (car lst) (looper (cdr lst) req-spaces))))))) 
       (list->string(looper (string->list string) (- width (string-length string)))))))

(define rj-helper
  (lambda (width string)
    (cond ((= width (string-length string)) string)
          (else  (rj-helper width (add-spaces width string))))))

(define rj-helper2
  (lambda (width ls)
    (cond ((null? ls) "")
          (else (string-append (rj-helper width (list->string(car ls))) (make-string 1 #\newline) (rj-helper2 width (cdr ls)))))))

(define list->stream
  (lambda (list)
	  (cond ((null? list) (stream))
		(else (stream-cons (car list)
				   (list->stream (cdr list)))))))

(define right-justify
  (lambda ( width stream)
    (list->stream(string->list(rj-helper2 width(seperate-lines(remove-end-spaces stream)))))))


(define sl-helper
  (lambda (stream)
    (letrec
        ((remove-loop
          (lambda (stream)
            (cond((stream-empty? stream) '())
                 ((eq? (stream-first stream) #\newline) '())
                 (else
                  (let ((ch (stream-first stream)))
                    (cons ch (remove-loop (stream-rest stream)))))))))
      (remove-loop stream))))

(define seperate-lines2
  (lambda (stream)
    (cond ((stream-empty? stream) '())
          ((stream-empty? (stream-rest stream)) '())
          ((eq? (stream-first stream) #\newline) (cons (sl-helper (stream-rest stream)) (seperate-lines2(stream-rest stream))))
          (else (seperate-lines2(stream-rest stream))))))

(define seperate-lines
  (lambda (stream)
    (let ((stream (remove-end-spaces stream)))
    (cons (sl-helper stream) (seperate-lines2 stream)))))

(define remove-extra-spaces
  (lambda (stream)
    (letrec
        ((remove-loop
          (lambda (stream)
            (cond ((stream-empty? stream) stream)
                  ((stream-empty? (stream-rest stream)) stream)
                  (else
                   (let ((ch (stream-first stream)))
                     (cond ((and (eq? ch (stream-first(stream-rest stream))) (eq? ch #\space)) (remove-loop (stream-rest stream)) )
                           (else
                            (stream-cons ch (remove-loop (stream-rest stream)))))))))))
      (remove-loop stream))))

(define remove-end-spaces
  (lambda (stream)
    (letrec
        ((remove-loop
          (lambda (stream)
            (cond ((stream-empty? stream) stream)
                  ((and (eq? #\space (stream-first stream)) (eq? #\newline (stream-first(stream-rest stream)))) (remove-loop (stream-rest stream)))
                  (else (stream-cons (stream-first stream) (remove-loop (stream-rest stream))))))))
      (remove-loop stream))))

(define remove-newlines
  (lambda (stream)
    (letrec
        ((remove-loop
          (lambda (stream)
            (cond ((stream-empty? stream) stream)
                  (else
                   (let ((ch (stream-first stream)))
                     (cond ((or (eq? #\newline ch) (eq? #\return ch)) (set! ch #\space)))
                     (stream-cons ch (remove-loop (stream-rest stream)))))))))
      (remove-loop stream))))

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

(define test-me
  (lambda (n)
    (formatter "test.txt" "out.txt" n)))
                            
                             