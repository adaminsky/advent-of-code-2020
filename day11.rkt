#lang racket

;; This is basically a 2d cellular automata.
(define (parse-program f)
  (for/vector ([l (in-lines f)])
    (list->vector (string->list l))))

(define (2dvector-ref vec x y)
  (vector-ref (vector-ref vec x) y))

;; Return the number of adjacent occupied seats given the position
;; (i, j) of a seat in cells.
(define (num-occupied-adj cells i j)
  (let ([h (vector-length cells)]
        [w (vector-length (vector-ref cells 0))])
    (+ (if (and (> i 0)
                (equal? #\# (2dvector-ref cells (- i 1) j)))
           1
           0)
       (if (and (> j 0)
                (equal? #\# (2dvector-ref cells i (- j 1))))
           1
           0)
       (if (and (< i (- h 1))
                (equal? #\# (2dvector-ref cells (+ i 1) j)))
           1
           0)
       (if (and (< j (- w 1))
                (equal? #\# (2dvector-ref cells i (+ j 1))))
           1
           0)
       (if (and (> i 0) (> j 0)
                (equal? #\# (2dvector-ref cells (- i 1) (- j 1))))
           1
           0)
       (if (and (> i 0) (< j (- w 1))
                (equal? #\# (2dvector-ref cells (- i 1) (+ j 1))))
           1
           0)
       (if (and (< i (- h 1)) (> j 0)
                (equal? #\# (2dvector-ref cells (+ i 1) (- j 1))))
           1
           0)
       (if (and (< i (- h 1)) (< j (- w 1))
                (equal? #\# (2dvector-ref cells (+ i 1) (+ j 1))))
           1
           0))))

(define (perform-update cells)
  (let ([h (vector-length cells)]
        [w (vector-length (vector-ref cells 0))])
    (for/vector ([i (in-range h)])
      (for/vector ([j (in-range w)])
        (cond [(and (equal? #\L (vector-ref (vector-ref cells i) j))
                    (eq? 0 (num-occupied-adj cells i j)))
               #\#]
              [(and (equal? #\# (vector-ref (vector-ref cells i) j))
                    (>= (num-occupied-adj cells i j) 4))
               #\L]
              [else (vector-ref (vector-ref cells i) j)])))))

(define (update-fixed-point f prev curr)
  (cond [(equal? prev curr) curr]
        [else (update-fixed-point f curr (f curr))]))

(define (count-occupied cells)
  (for*/fold ([cnt 0])
             ([i (in-range (vector-length cells))]
              [j (in-range (vector-length (vector-ref cells 0)))])
    (cond [(equal? #\# (2dvector-ref cells i j)) (+ cnt 1)]
          [else cnt])))

(define (occupied-in-slope cells x y rise run)
  (cond [(and (>= (+ x rise) 0) (>= (+ y run) 0)
              (< (+ x rise) (vector-length cells))
              (< (+ y run) (vector-length (vector-ref cells 0))))
         (cond [(equal? #\# (2dvector-ref cells (+ x rise) (+ y run))) 1]
               [(equal? #\L (2dvector-ref cells (+ x rise) (+ y run))) 0]
               [else (occupied-in-slope cells (+ x rise) (+ y run) rise run)])]
        [else 0]))

(define (num-occupied-slope cells i j)
  (+ (occupied-in-slope cells i j 0 1)
     (occupied-in-slope cells i j 1 0)
     (occupied-in-slope cells i j 0 -1)
     (occupied-in-slope cells i j -1 0)
     (occupied-in-slope cells i j -1 -1)
     (occupied-in-slope cells i j -1 1)
     (occupied-in-slope cells i j 1 -1)
     (occupied-in-slope cells i j 1 1)))

(define (perform-update2 cells)
  (let ([h (vector-length cells)]
        [w (vector-length (vector-ref cells 0))])
    (for/vector ([i (in-range h)])
      (for/vector ([j (in-range w)])
        (cond [(and (equal? #\L (vector-ref (vector-ref cells i) j))
                    (eq? 0 (num-occupied-slope cells i j)))
               #\#]
              [(and (equal? #\# (vector-ref (vector-ref cells i) j))
                    (>= (num-occupied-slope cells i j) 5))
               #\L]
              [else (vector-ref (vector-ref cells i) j)])))))

(call-with-input-file "day11_in.txt"
  (lambda (f)
    (let* ([cells (parse-program f)]
           [h (vector-length cells)]
           [w (vector-length (vector-ref cells 0))])
      (count-occupied
       (update-fixed-point perform-update2 '() cells)))))
