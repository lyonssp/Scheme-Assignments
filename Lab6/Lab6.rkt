#lang racket

(define point 
  (lambda (x y)
    (list x y)))

(define rectangle
  (lambda (point1 point2)
    (list point1 point2)))

(define polygon
  (lambda (point1 point2 . additional-points)
    (letrec
        ((helper
          (lambda (points) 
          (if (null? points)
              '()
              (append (list (car points))
                      (helper (cdr points)))))))
    (append (list point1 point2) (helper additional-points)))))