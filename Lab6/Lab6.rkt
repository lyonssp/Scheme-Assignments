#lang racket/gui
(require racket/class)


;Frame axis is configured with the origin in the top left corner
;of the window and the coordinate containing the x and y boundaries
;in the bottom right corner of the window
(define frame (new frame%
                   (label "Canvas")
                   (width 500)
                   (height 500)
                   ))

(define draw-point
  (lambda (point)
    (send dc set-pen "white" 1 'solid)
    (send dc draw-point (send point get-x) (send point get-y))))

(define point-obj
  (class object%
    (super-new)
    (init-field x y [depth 1])
    
    ;return x coordinate of point
    (define/public get-x
      (lambda ()
        x))
    
    ;return y coordinate of point
    (define/public get-y
      (lambda ()
        y))

    ;what type of object is this
    (define/public type?
      (lambda ()
      'point))
    
    ;return depth in comparison to other objects
    (define/public depth?
      (lambda ()
        depth))
    
    ;will draw a point
    (define/public draw
      (lambda ()
        (send dc set-pen "white" 1 'solid)
        (send dc draw-point (get-x) (get-y))))
  ))

(define draw
  (lambda (object)
    (send object draw)))

(define type
  (lambda (object)
    (send object type?)))

(define depth
  (lambda (object)
    (send object depth?)))

(define canvas (new canvas% (parent frame)
             (paint-callback
              (lambda (canvas dc)
                (send dc set-background "black")
                (send dc clear)
                ))
             ))

(define dc (send canvas get-dc))
(send frame show #t)
(sleep/yield 1)

(define point
  (lambda (x y (depth 1))
    (new point-obj (x x) (y y) (depth depth))))

(define point?
  (lambda (object)
    (cond ((equal? (send object type?) 'point) #t)
          (else #f))))

(define rectangle%
  (class object%
    (super-new)
    (init-field x y (color '()) [depth 1])
    (define/public get-x
      (lambda ()
        (send x get-x)))
    (define/public get-y
      (lambda ()
        (send x get-y)))
    (define/public get-width
      (lambda ()
        (abs(- (send x get-x) (send y get-x)))))
    (define/public get-height
      (lambda ()
        (abs(- (send x get-y) (send y get-y)))))
    (define/public get-color
      (lambda ()
        color))
    (define/public type?
      (lambda ()
      'rectangle))
    (define/public depth?
      (lambda ()
        depth))
    (define/public draw
      (lambda ()
        (cond ((not(string? (get-color))) (send dc set-pen "white" 1 'solid) (send dc set-brush "white" 'transparent) 
                                        (send dc draw-rectangle (get-x) (get-y) (get-width) (get-height))) 
              (else (send dc set-pen (get-color) 1 'solid) (send dc set-brush (get-color) 'solid)
                    (send dc draw-rectangle (get-x) (get-y) (get-width) (get-height))))))
  ))

(define rectangle
  (lambda (x y (color '()) (depth 1))
    (new rectangle% (x x) (y y) (color color) (depth depth))))

(define draw-rectangle
  (lambda (rectangle-obj)
    (cond ((equal? (send rectangle-obj get-color) '()) (send dc set-pen "white" 1 'solid) (send dc set-brush "white" 'transparent) 
                                                       (send dc draw-rectangle (send rectangle-obj get-x) (send rectangle-obj get-y) (send rectangle-obj get-width) (send rectangle-obj get-height))) 
          (else (send dc set-pen (send rectangle-obj get-color) 1 'solid) (send dc set-brush (send rectangle-obj get-color) 'solid)
                (send dc draw-rectangle (send rectangle-obj get-x) (send rectangle-obj get-y) (send rectangle-obj get-width) (send rectangle-obj get-height))))
    (send dc draw-rectangle (send rectangle-obj get-x) (send rectangle-obj get-y) (send rectangle-obj get-width) (send rectangle-obj get-height))))
    
(define rectangle?
  (lambda (object)
    (cond ((equal? (send object type?) 'rectangle) #t)
          (else #f))))

(define scene
  (lambda (objs)
    (new scene% (objects objs))))

(define polygon-obj
  (class object%
    (super-new)
    (init-field points (color '()) (depth 1))

    (define/public get-points
      (lambda ()
        points))
    
    (define/public get-color
      (lambda ()
        (cond ((null? color) '())
              (else (car color)))))

    (define/public type?
      (lambda ()
      'polygon))
    
    (define/public depth?
      (lambda ()
        depth))
    
    (define/public draw
      (lambda ()
        (cond ((equal? (get-color) '()) (send dc set-pen "white" 1 'solid) (send dc set-brush "white" 'transparent) 
                                                       (send dc draw-polygon (get-points))) 
          (else (send dc set-pen (get-color) 1 'solid) (send dc set-brush (get-color) 'solid)
                (send dc draw-polygon (get-points))))
    ))
  ))

(define polygon
  (lambda vars
    (define color '())
    (define depth 1)
    (define createlist
      (lambda (vars)
        (cond ((null? vars) '())
              ((string? (car vars)) (set! color vars) (createlist (cdr vars)) '())
              ((not(object? (car vars))) (set! depth (car vars)) '())
              (else
               (cons (cons (send (car vars) get-x) (send (car vars) get-y)) (createlist (cdr vars))))
              )))
    (new polygon-obj (points (createlist vars)) (color color) (depth depth))
              ))

(define draw-polygon
  (lambda (poly)
    (cond ((equal? (send poly get-color) '()) (send dc set-pen "white" 1 'solid) (send dc set-brush "white" 'transparent) 
                                                       (send dc draw-polygon (send poly get-points))) 
          (else (send dc set-pen (send poly get-color) 1 'solid) (send dc set-brush (send poly get-color) 'solid)
                (send dc draw-polygon (send poly get-points))))
    ))

(define polygon?
  (lambda (object)
    (cond ((equal? (send object type?) 'polygon) #t)
          (else #f))))

(define scene%
  (class object%
    (super-new)
    (init-field objects)
    
    (define/public type?
      (lambda ()
      'scene))
    
    (define lower-depth?
      (lambda (a b)
        (cond ((< (depth a) (depth b)) #t)
          (else #f))))
    
    (define/public draw
      (lambda ()
        (for-each (lambda (arg) (send arg draw)) (sort objects lower-depth?))
        ))
    ))



(define animate
 (lambda ()
   (define -scene- (scene (list (rectangle (point 50 300) (point 450 400) "yellow" 2) ;;crown base
                            (rectangle (point 75 200) (point 125 300) "yellow" 2) ;;
                            (rectangle (point 50 225) (point 150 250) "yellow" 2) ;;left crown perch
                            (rectangle (point 375 200) (point 425 300) "yellow" 2) ;;
                            (rectangle (point 350 225) (point 450 250) "yellow" 2) ;;right crown perch
                            (rectangle (point 200 150) (point 300 300) "yellow" 2) ;;
                            (rectangle (point 175 200) (point 325 250) "yellow" 2) ;;center crown perch
                            (polygon   (point 75 235) (point 100 210) (point 125 235) (point 100 260) "purple" 3);left perch jewel
                            (polygon   (point 375 235) (point 400 210) (point 425 235) (point 400 260) "purple" 3);right perch jewel
                            (polygon   (point 200 225) (point 250 150) (point 300 225) (point 250 300) "red" 3);;center perch jewel
                            (polygon   (point 225 350) (point 250 325) (point 275 350) (point 250 375) "blue" 3);;left base jewel
                            (polygon   (point 75 350) (point 100 325) (point 125 350) (point 100 375) "blue" 3);;center base jewel
                            (polygon   (point 375 350) (point 400 325) (point 425 350) (point 400 375) "blue" 3);;right base jewel
                            ))) 
   (draw -scene-)))

(animate)