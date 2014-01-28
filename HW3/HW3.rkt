#lang racket/gui

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Sliding Puzzle Solver
;TO RUN:  Just compile the code.  The board will appear, it will scramble, then pause, then solve.
;
;Known Issues: Solves by making random moves from a scrambled configuration.  
;              This method is guaranteed to solve the puzzle, but there is
;              no guarantee it will solve efficiently
;
;
;NOTE: The solving process can be slowed down by editing the sleep time in the 
;      randomize method of class board.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define frame (new frame%
                   [label "8 Puzzle"]
                   [width 300]
                   [height 330]
                   [style (list 'no-resize-border)]
                   ))

(define canvas (new canvas% [parent frame]
                    [paint-callback
                     (lambda (canvas dc)
                       (send dc set-background "green")
                       (send dc clear)
                       )]))

(define dc (send canvas get-dc))

(send frame show #t)
(sleep/yield 1)

(define fill
  (lambda (box num)
    (send dc set-brush "white" 'solid)
    (cond ((= 0 num) 
           (cond ((= 0 box) (send dc draw-rectangle 0 0 100 100))
                 ((= 1 box) (send dc draw-rectangle 100 0 100 100))
                 ((= 2 box) (send dc draw-rectangle 200 0 100 100))
                 ((= 3 box) (send dc draw-rectangle 0 100 100 100))
                 ((= 4 box) (send dc draw-rectangle 100 100 100 100))
                 ((= 5 box) (send dc draw-rectangle 200 100 100 100))
                 ((= 6 box) (send dc draw-rectangle 0 200 100 100))
                 ((= 7 box) (send dc draw-rectangle 100 200 100 100))
                 ((= 8 box) (send dc draw-rectangle 200 200 100 100))
                 ))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;NOTE: values d and a are not used below.  
          ;      They prevent a contract failure for define-values.
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
          (else
           (cond ((= 0 box) (define-values (text-width text-height m n) (send dc get-text-extent (number->string num))) 
                            (send dc draw-text (number->string num) (/ (- 100 text-width) 2) (/ (- 100 text-height) 2)))
                 ((= 1 box) (define-values (text-width text-height m n) (send dc get-text-extent (number->string num))) 
                            (send dc draw-text (number->string num) (/ (- 300 text-width) 2) (/ (- 100 text-height) 2)))
                 ((= 2 box) (define-values (text-width text-height m n) (send dc get-text-extent (number->string num))) 
                            (send dc draw-text (number->string num) (/ (- 500 text-width) 2) (/ (- 100 text-height) 2)))
                 ((= 3 box) (define-values (text-width text-height m n) (send dc get-text-extent (number->string num))) 
                            (send dc draw-text (number->string num) (/ (- 100 text-width) 2) (/ (- 300 text-height) 2)))
                 ((= 4 box) (define-values (text-width text-height m n) (send dc get-text-extent (number->string num)))
                            (send dc draw-text (number->string num) (/ (- 300 text-width) 2) (/ (- 300 text-height) 2)))
                 ((= 5 box) (define-values (text-width text-height m n) (send dc get-text-extent (number->string num))) 
                            (send dc draw-text (number->string num) (/ (- 500 text-width) 2) (/ (- 300 text-height) 2)))
                 ((= 6 box) (define-values (text-width text-height m n) (send dc get-text-extent (number->string num))) 
                            (send dc draw-text (number->string num) (/ (- 100 text-width) 2) (/ (- 500 text-height) 2)))
                 ((= 7 box) (define-values (text-width text-height m n) (send dc get-text-extent (number->string num)))
                            (send dc draw-text (number->string num) (/ (- 300 text-width) 2) (/ (- 500 text-height) 2)))
                 ((= 8 box) (define-values (text-width text-height m n) (send dc get-text-extent (number->string num))) 
                            (send dc draw-text (number->string num) (/ (- 500 text-width) 2) (/ (- 500 text-height) 2)))
                 )))))
  
(define board%
  (class object%
    (super-new)
    
    (define board-vector
      (list->vector '(0 1 2 3 4 5 6 7 8))
      )
    
    (define/public vector-swap
      (lambda (index1 index2)
        (let ((temp (vector-ref board-vector index1)))
          (vector-set! board-vector index1
                       (vector-ref board-vector index2))
          (vector-set! board-vector index2
                       temp))))
    
    (define/public check-down
      (lambda ()
        (cond((> (vector-member 0 board-vector) 5) #f)
             (else #t))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;prevent blank tile from moving off the board
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public check-up
      (lambda ()
        (cond((< (vector-member 0 board-vector) 3) #f)
             (else #t))))
    
    (define/public check-left
      (lambda ()
        (cond((= (modulo (vector-member 0 board-vector) 3) 0) #f)
             (else #t))))
      
    (define/public check-right
      (lambda ()
        (cond((= (modulo (vector-member 0 board-vector) 3) 2) #f)
             (else #t))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;Move blank tile
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public move-left
      (lambda ()
        (cond ((check-left) (vector-swap (vector-member 0 board-vector) (- (vector-member 0 board-vector) 1)))
              (else '()))
        (redraw)))
    
    (define/public move-right
      (lambda ()
        (cond ((check-right) (vector-swap (vector-member 0 board-vector) (+ (vector-member 0 board-vector) 1)))
              (else '()))
        (redraw)))
    
    (define/public move-up
      (lambda ()
        (cond ((check-up) (vector-swap (vector-member 0 board-vector) (- (vector-member 0 board-vector) 3)))
              (else '()))
        (redraw)))
    
    (define/public move-down
      (lambda ()
        (cond ((check-down) (vector-swap (vector-member 0 board-vector) (+ (vector-member 0 board-vector) 3)))
              (else '()))
        (redraw)))
    
    (define/public redraw
      (lambda ()
        (send dc clear)
        (send dc set-font (make-font #:size 50 #:weight 'normal #:style 'italic))
        
        ;Grid Lines on Board
        (send dc draw-line 100 0 100 300)
        (send dc draw-line 200 0 200 300)
        (send dc draw-line 0 100 300 100)
        (send dc draw-line 0 200 300 200)
        
        (define init-boxes
          (lambda (vec box)
            (cond ((not(null? vec)) 
                   (fill box (car vec)) 
                   (init-boxes (cdr vec) (+ box 1))))))
        
        (init-boxes (vector->list board-vector) 0)
        ))
    
    ;Scramble the Board
    (define/public randomize
      (lambda (times)
        (cond ((> times 0) (define rand-move (random 4))
                          (cond ((and (= rand-move 0) (check-up)) (move-up)) 
                                ((and (= rand-move 1) (check-right)) (move-right))
                                ((and (= rand-move 2) (check-down)) (move-down))
                                ((and (= rand-move 3) (check-left)) (move-left))
                                (else (randomize 1)))
                          (sleep .005) ;;EDIT THIS TO CHANGE TIME BETWEEN MOVES
                          (randomize (- times 1))))))
    
    ;Solve puzzle by randomly moving the blank until solved.
    (define/public solve
      (lambda ()
        (cond ((equal? (vector->list board-vector) '(0 1 2 3 4 5 6 7 8)) (print "Solved"))
              (else
               (define rand (random 4))
               (cond ((and (= rand 0) (check-up)) (move-up))
                     ((and (= rand 1) (check-right)) (move-right))
                     ((and (= rand 2) (check-down)) (move-down))
                     ((and (= rand 3) (check-left)) (move-left))
                     (else (randomize 1)))
               (solve)))))
    
    ))

(define puzzle (new board%))
(send puzzle redraw)

(define animate-solve
  (lambda ()
     (displayln "Generating Board")
     (sleep 2)
     (send puzzle randomize 100)
     (displayln "Solving")
     (sleep 2)
     (send puzzle solve)))

(animate-solve)