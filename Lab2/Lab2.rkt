#lang racket

(define accumulate
  (lambda (op base term ls)
    (if (null? ls)
        base
        (op (term (car ls))
            (accumulate op base term (cdr ls))))))

(define length
  (lambda (ls)
    (accumulate + 0 (lambda(x) 1) ls)))

(define append
  (lambda (ls1 ls2)
    (accumulate cons ls2 (lambda(x) x) ls1)))

(define reverse
  (lambda (ls)
    (if (null? ls)
        '()
    (append (reverse (cdr ls)) (cons (car ls) '())))))

(define prefix?
  (lambda (ls1 ls2)
    (cond ((null? ls1) #t)
          ((null? ls2) #f)
          ((equal? (car ls1) (car ls2)) (prefix? (cdr ls1) (cdr ls2)))
          (else #f))))

(define subsequence?
  (lambda (ls1 ls2)
    (cond ((null? ls2) #f)
          ((prefix? ls1 ls2) #t)
          ((subsequence? ls1 (cdr ls2)))
          (else #f))))
    

(define sublist?
  (lambda (ls1 ls2)
    (cond ((null? ls1) #t)
          ((subsequence? (cons (car ls1) '()) ls2) (sublist? (cdr ls1) ls2))
           (else #f))))

(define map
  (lambda (f ls)
    (accumulate cons '() f ls)))

(define filter
  (lambda ( test? ls)
    (cond ((null? ls) '())
        ((test? (car ls)) (cons (car ls) (filter test? (cdr ls))))
        (else (filter test? (cdr ls))))))

(define insert1
  (lambda (item sorted_ls)
    (cond ((null? sorted_ls) (list item))
          ((> item (car sorted_ls)) (cons (car sorted_ls) (insert1 item (cdr sorted_ls))))
           (else (cons item sorted_ls)))))

(define num-sort
  (lambda (ls)
    (if (null? ls)
        '()
        (insert1 (car ls)
                (num-sort (cdr ls))))))

(define insert2
  (lambda (item sorted-ls less?)
    (cond ((null? sorted-ls) (list item))
          ((less? item (car sorted-ls))(cons item sorted-ls))      
          (else (cons (car sorted-ls) (insert2 item (cdr sorted-ls) less?))))))

(define sort
  (lambda (ls less?)
    (if (null? ls)
        '()
        (insert2 (car ls)
                 (sort (cdr ls) less?)
                 less?))))

(define make-sort
  (lambda (less?)
    (lambda (ls)
      (if (null? ls) '()
          (insert1 (car ls)
                   (sort (cdr ls) less?))))))


          
          

     
    
  
         