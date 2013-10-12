#lang racket

(define accumulate
  (lambda (op base term ls)
    (if (null? ls)
        base
        (op (term (car ls))
            (accumulate op base term (cdr ls))))))

(define mymap
  (lambda (f ls)
    (accumulate cons '() f ls)))

(define length
  (lambda (ls)
    (accumulate + 0 (lambda (x) 1) ls)))

(define append
  (lambda (ls1 ls2)
    (accumulate cons ls2 (lambda(x) x) ls1)))

(define insert1
  (lambda (item sorted_ls)
    (cond ((null? sorted_ls) (list item))
          ((> item (car sorted_ls)) (cons (car sorted_ls) (insert1 item (cdr sorted_ls))))
           (else (cons item sorted_ls)))))

(define num-sort
  (lambda (ls)
    (if (null? ls)
        '()
        (insert (car ls)
                (num-sort (cdr ls))))))

        