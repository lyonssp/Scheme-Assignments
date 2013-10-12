#lang racket


#|
(define smart-product
  (lambda (ls)
    (call-with-current-continuation
     (lambda (cc)
            (let helper ((ls ls))
              (cond ((null? ls) 1)
                    ((zero? (car ls)) (cc 0))
                    (else
                     (ping "down" (car ls))
                     (let ((answer ((helper (csr ls)))))
                       (ping "up" (car ls))
                       (* (car ls) answer)))))))))|#
                     
(define zombie #f)
(define factorial
  (lambda (n)
    (cond ((zero? n)
          (call-with-current-continuation
           (lambda (cc)
             (set! zombie cc)
             1)))
          (else (* n (factorial (- n 1)))))))

(define car&cdr
  (lambda (ls cc)
    (cc (car ls) (cdr ls))))

(define quotient-remainder
  (lambda (a b success failure)
    (cond ((zero? b) 
           (failure "division by zero"))
          (else 
           (success (quotient a b) (remainder a b))))))

(define imp-cc
  (lambda ()
    (letrec ((f (lambda (x) (cons 'a x)))
             (g (lambda (x) (cons 'b (f x))))
             (h (lambda (x) (g (cons 'c x)))))
      (cons 'd (h '())))))

(define exp-cc
  (lambda ()
    (letrec
        ((f (lambda (x cc) (cc (cons 'a x))))
         (g (lambda (x cc) (f x (lambda (y) (cc (cons 'b y))))))
         (h (lambda (x cc) (g (cons 'c x) cc))))
    (h '() (lambda (y) (cons 'd y))))))
         
             