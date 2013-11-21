#lang racket

(define writeln
  (lambda (sentence)
    (write sentence)))

(define accumulate
  (lambda (op base term ls)
    (if (null? ls)
        base
        (op (term (car ls))
            (accumulate op base term (cdr ls))))))

(define length
  (lambda (ls)
    (accumulate + 0 (lambda(x) x) ls)))

;Hedging Operation Functions:

(define *hedges*
	  '((its like im telling you)
	    (now calm down)
	    (take it easy)
	    (its elementary lou)
	    (im trying to tell you)
	    (but you asked)))

;List indexing function
(define list-ref
  (lambda (ls index)
    (cond ((null? ls) '())
          ((zero? index) (car ls))
          (else (list-ref (cdr ls) (- index 1))))))

;List select function
(define select-any-from-list
  (lambda (ls)
    (list-ref ls (random (length ls)))))

;Ending Operation Functions:
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

(define any-good-fragments?
  (lambda (list-of-cues sentence)
    (cond ((null? list-of-cues) #f)
          ((subsequence? (car list-of-cues) sentence) #t)
          (else (any-good-fragments? (cdr list-of-cues) sentence)))))

;Strong Replies
(define *strong-cues*
	  '( ( ((the names) (their names))
	       ((whos on first whats on second i dont know on third)
 	        (whats on second whos on first i dont know on third)) )

	     ( ((suppose) (lets say) (assume))
	       ((okay) (why not) (sure) (it could happen)) )

	     ( ((i dont know))
	       ((third base) (hes on third)) )
	   ))

(define cue-part
  (lambda (pair)
    (car pair)))

(define reponse-part
  (lambda (pair)
    (cadr pair)))

(define try-strong-cues
  (lambda (sentence)
    (define helper
      (lambda (list-of-pairs)
        (cond ((null? list-of-pairs) '())
              ((any-good-fragments? (cue-part list-of-pairs) sentence))
              (select-any-from-list (reponse-part (car list-of-pairs)))
              (else (helper (cdr list-of-pairs))))))
    (helper *strong-cues*)))

;Weak Replies
(define *weak-cues*
  '( ( ((who) (whos) (who is))
       ((first-base)
           ((thats right) (exactly) (you got it)
	    (right on) (now youve got it)))
       ((second-base third-base)
           ((no whos on first) (whos on first) (first base))) )
     ( ((what) (whats) (what is))
       ((first-base third-base)
	   ((hes on second) (i told you whats on second)))
       ((second-base)
	   ((right) (sure) (you got it right))) )
     ( ((whats the name))
       ((first-base third-base)
	   ((no whats the name of the guy on second)
	    (whats the name of the second baseman)))
       ((second-base)
	((now youre talking) (you got it))))
   ))

(define try-weak-cues
  (lambda (sentence)
    (define helper
      (lambda (list-of-pairs)
        (cond ((null? list-of-pairs) '())
              ((any-good-fragments? (cue-part list-of-pairs) sentence))
              (select-any-from-list (reponse-part (car list-of-pairs)))
              (else (helper (cdr list-of-pairs))))))
    (helper *weak-cues*)))

;Context Functions
(define whos
  (lambda ()
    (whos-on-first-loop '())))
             
(define get-context
  (lambda (sentence old-context)
    '()))

(define wants-to-end?
  (lambda (sentence)
    (equal? sentence '(i quit))))

(define wrap-it-up
  (lambda ()
    (writeln
    exit)))
    
                    
(define hedge
  (lambda ()
            (select-any-from-list *hedges*)))

(define whos-on-first-loop 
	  (lambda (old-context)
	    (let ((costellos-line (read)))
	      (let ((new-context (get-context costellos-line old-context)))
	        (let ((strong-reply (try-strong-cues costellos-line)))
	          (let ((weak-reply (try-weak-cues costellos-line new-context))) 
	               (cond ((not (null? strong-reply))
	                      (writeln strong-reply)
	                      (whos-on-first-loop (get-context strong-reply new-context)))
	                     ((not (null? weak-reply))
	                      (writeln weak-reply)
			      (whos-on-first-loop (get-context weak-reply new-context)))
	                     ((wants-to-end? costellos-line)
	                      (wrap-it-up))
	                     (else 
	                      (writeln (hedge))
	                      (whos-on-first-loop new-context)))))))))
