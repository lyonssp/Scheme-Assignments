#lang racket

(define writeln
  (lambda (stuff)
    (write stuff)
    (newline)))

#|(define make-stack
  (lambda ()
    (let ((stack '()))
      (letrec
          ((this
            (lambda message
              (cond ((null? message) (writeln "No Arguments"))
                     (else
                      (case (car message)
                        ((show) stack)
                        ((empty?) (null? stack))
                        ((error) (writeln "Error") (writeln (cadr message)))
                        (else (writeln "Uknown Message"))))))))
        this))))|#

#|(define make-stack
  (lambda ()
    (let ((stack '()))
      (letrec
          ((this
            (lambda message
              (cond ((null? message) (this 'error "No Arguments"))
                     (else
                      (case (car message)
                        ((show) stack)
                        ((empty?) (null? stack))
                        ((error) 
                         (if (not (null? (cdr message)))
                             (writeln (cadr message))
                             (this 'error "No argument to error!")))
                        (else (writeln "Uknown Message"))))))))
        this))))|#

(define make-stack
  (lambda ()
    (let ((stack '()))
      (letrec
          ((this
            (lambda message
              (cond ((null? message) (this 'error "No Arguments"))
                     (else
                      (case (car message)
                        ((show) stack)
                        ((empty?) (null? stack))
                        ((top) (if (not (this 'empty?)) 
                                   (car stack) 
                                   (this 'error "Empty Stack")))
                        ((push!)
                         (if (not (null? (cdr message)))
                             (set! stack (cons (cadr message) stack))
                             (this 'error "Nothing to push")))
                        ((pop!)
                         (if (not (this 'empty?))
                             (set! stack (cdr stack))
                             (this 'error "Nothing to pop")))
                        ((clear!)
                         (let loop ()
                           (if (not (this 'empty?))
                               (begin 
                                 (this 'pop!)
                                 (loop))
                               (this 'error "Done clearing stack"))))
                                       
                        ((error) 
                         (if (not (null? (cdr message)))
                             (writeln (cadr message))
                             (this 'error "No argument to error!")))
                        (else (writeln "Uknown Message"))))))))
        this))))