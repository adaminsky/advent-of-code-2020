#lang racket

(require racket/set)

(struct point (x y z)
  #:transparent)

(define (2dvector-ref vec x y)
  (vector-ref (vector-ref vec x) y))

(define (parse-program f)
  (for/vector ([l (in-lines f)])
    (list->vector (string->list l))))

(define (initial->active state)
  (for*/fold ([active (set)])
             ([i (range (vector-length state))]
              [j (range (vector-length (vector-ref state 0)))])
    (if (equal? #\. (2dvector-ref state i j))
        active
        (set-add active (point i j 0)))))

(define (active-area active)
  (for/fold ([dims (list 0 0 0)])
            ([cube active])
    (let ([new-x (if (> (abs (point-x cube)) (car dims))
                     (abs (point-x cube))
                     (car dims))]
          [new-y (if (> (abs (point-y cube)) (cadr dims))
                     (abs (point-y cube))
                     (cadr dims))]
          [new-z (if (> (abs (point-z cube)) (caddr dims))
                     (abs (point-z cube))
                     (caddr dims))])
      (list new-x new-y new-z))))

(define (find-pocket active)
  (let ([active-area (active-area active)])
    (map (lambda (x) (+ x 1)) active-area)))

(define (get-neighbor-cubes pt)
  (let ([all (for*/fold ([neighbors (set)])
                        ([i (list -1 0 1)]
                         [j (list -1 0 1)]
                         [k (list -1 0 1)])
               (set-add neighbors (struct-copy point pt
                                               [x (+ (point-x pt) i)]
                                               [y (+ (point-y pt) j)]
                                               [z (+ (point-z pt) k)])))])
    (set-remove all pt)))

(define (count-active-neighbors active pt)
  (let ([neighbors (get-neighbor-cubes pt)])
    (count-active (set-intersect active neighbors))))

(define (cycle active)
  (let ([pocket (find-pocket active)])
    (for*/fold ([new-active active])
               ([i (range (- 0 (car pocket)) (+ 1 (car pocket)))]
                [j (range (- 0 (cadr pocket)) (+ 1 (cadr pocket)))]
                [k (range (- 0 (caddr pocket)) (+ 1 (caddr pocket)))])
      (let ([local_cnt (count-active-neighbors active (point i j k))]
            [cube_active (set-member? active (point i j k))])
        (cond [(and cube_active
                    (and (not (equal? 2 local_cnt))
                        (not (equal? 3 local_cnt))))
               (set-remove new-active (point i j k))]
              [(and (not cube_active)
                    (equal? 3 local_cnt))
               (set-add new-active (point i j k))]
              [else new-active])))))

(define (six-cycles active i)
  (if (< i 6)
      (six-cycles (cycle active) (+ i 1))
      active))

(define (count-active active)
  (set-count active))

(call-with-input-file "day17_in.txt"
  (lambda (f)
    (let ([init_active (initial->active (parse-program f))])
      (count-active (six-cycles init_active 0)))))
