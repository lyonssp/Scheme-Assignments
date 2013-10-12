#lang racket

;For testing
(define square
  (lambda (n)
    (* n n)))

;Problem 1
(define triangular
  (lambda (n)
    (/ (* n (+ n 1)) 2)))

;Problem 2
(define square-triangular
  (lambda (n)
    (/ (* n (+ n 1) (+ (* 2 n) 1)) 6)))

;Problem 3
(define generalized-triangular
  (lambda (n f)
    (if (= n 1)
        (f 1)
        (+ (f n) (generalized-triangular (- n 1) f)))))

;Problem 4
;Do not understand syntax error
;(define generic-triangular
;  (lambda (f)
;    (letrec ([helper
;              lambda (n)]
;             (lambda (n)
;               (if (= n 0)
;                   0
;                   ((+ (f n) helper (- n 1)))))))))
       
      