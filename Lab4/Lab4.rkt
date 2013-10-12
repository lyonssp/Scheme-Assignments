#lang racket

(define accumulate
  (lambda (op base term ls)
    (if (null? ls)
        base
        (op (term (car ls))
            (accumulate op base term (cdr ls))))))

(define prefix?
  (lambda (ls1 ls2)
    (cond ((null? ls1) #t)
          ((null? ls2) #f)
          ((equal? (car ls1) (car ls2)) (prefix? (cdr ls1) (cdr ls2)))
          (else #f))))

(define exist?
  (lambda (element ls2)
    (cond ((null? ls2) #f)
          ((prefix? (list element) ls2) #t)
          (else (exist? element (cdr ls2))))))
              

(define remove-duplicates
  (lambda (ls)
    (cond ((null? ls) '())
          ((exist? (car ls) (cdr ls)) 
           (remove-duplicates (cdr ls)))
          (else (cons (car ls) (remove-duplicates (cdr ls)))))))

(define set1?
  (lambda (ls)
    (equal? ls (remove-duplicates ls))))

(define set2?
  (lambda (ls)
    (cond ((null? ls) #t) 
          ((exist? (car ls) (cdr ls)) #f)
          (else (set2? (cdr ls))))))
            
(define union1
  (lambda (ls1 ls2)
    (remove-duplicates (append ls1 ls2))))

(define union2
  (lambda (ls1 ls2)
    (remove-duplicates (accumulate append ls2 (lambda (x) (list x)) ls1))))

(define union3
  (lambda (ls1 ls2)
    (if (null? ls1) 
        ls2
        (remove-duplicates (append (list (car ls1)) (union3 (cdr ls1) ls2))))))

(define subset?
  (lambda (ls1 ls2)
    (accumulate (lambda (a b) (and a b)) #t (lambda (x) (exist? x ls2)) ls1)))

(define subset2?
  (lambda (ls1 ls2)
    (equal? (union1 ls1 ls2) ls2)))

(define subset3?
  (lambda (ls1 ls2)
   (cond ((null? ls1) #t)
         ((not (exist? (car ls1) ls2)) #f)
         (else (subset3? (cdr ls1) ls2)))))