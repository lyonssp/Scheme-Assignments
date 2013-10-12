#lang racket
(require racket/stream)

(define list->file
  (lambda (filename ls)
    (let ((output-port (open-output-file filename)))
      (letrec
          ((build-output-file
            (lambda (ls)
              (cond ((null? ls) (close-output-port output-port))
                    (else 
                     (write-char (car ls) output-port)
                     (build-output-file (cdr ls)))))))
  (build-output-file ls)))))


(define stream->file
  (lambda (filename stream)
     (let ((output-port (open-output-file filename)))
      (letrec
          ((build-output-file
            (lambda (stream)
              (cond ((stream-empty? stream) (close-output-port output-port))
                    (else 
                     (write-char (stream-first stream) output-port)
                     (build-output-file (stream-rest stream)))))))
  (build-output-file stream)))))




;STREAM MANIPULATORS AND MUTATORS
(define freeze
  (lambda (expr)
    (lambda ()
      expr)))

;Get first element of stream with (stream-first naturals)
;Get everything but first element of stream with (stream-rest naturals)
(define naturals
  (let run4ever ((n 0))
    (stream-cons n
                 (run4ever (+ n 1)))))

(define my-stream->list
  (lambda (str num)
    (cond ((stream-empty? str) '()) ;Only worth checking when you are unsure about nature of stream
          ((zero? num) '())
          (else (cons (stream-first str) (my-stream->list (stream-rest str) (- num 1)))))))

(define my-stream-map
  (lambda (f str)
    (cond ((stream-empty? str) empty-stream)
          (else (stream-cons (f (stream-first str))
                              (my-stream-map f (stream-rest str)))))))

(define my-stream-filter
  (lambda (test? str)
    (cond ((stream-empty? str) empty-stream)
          ((test? (stream-first str)) 
           (stream-cons (stream-first str)
                        (my-stream-filter test? (stream-rest str))))
          (else (my-stream-filter test? (stream-rest str))))))

(define fibs
  (let run4ever ((n 0) (m 1))
    (stream-cons n
                 (run4ever m (+ n m)))))

(define my-stream-operator
  (lambda (op str1 str2)
    (cond ((stream-empty? str1) empty-stream)
          ((stream-empty? str2) empty-stream)
          (else (stream-cons (op (stream-first str1) (stream-first str2))
                             (my-stream-operator op (stream-rest str1) (stream-rest str2)))))))
          