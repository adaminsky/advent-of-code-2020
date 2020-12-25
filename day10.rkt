#lang racket

(define (parse-program f)
  (for/list ([l (in-lines f)])
    (string->number l)))

(define (all-adapters-order input)
  (sort input <))

(define (get-differences input)
  (let ([prev (cons 0 input)])
    (for/list ([i input]
               [j prev])
      (- i j))))

(define (count-n dif n)
  (for/fold ([cnt 0])
            ([d dif])
    (if (eq? d n) (+ cnt 1) cnt)))

(define (find-free dif)
  (for/fold ([free '(0)])
            ([d dif])
    (cond [(eq? d 1) (cons (+ (car free) 1) (cdr free))]
          [(eq? (car free) 0) free]
          [(eq? (car free) 1) (cons 0 (cdr free))]
          [else (cons 0 (cons (- (car free) 1) (cdr free)))])))

(call-with-input-file "day10_in.txt"
  (lambda (f)
    (let* ([diff (get-differences (all-adapters-order (parse-program f)))]
           [num-1 (count-n diff 1)]
           [num-3 (count-n diff 3)])
      (* num-1 (+ 1 num-3)))))

(define (count-arrangements free)
  (for/fold ([arrange 1])
            ([f free])
    (cond [(eq? f 1) (* 2 arrange)]
          [(eq? f 2) (* 4 arrange)]
          [(eq? f 3) (* 7 arrange)])))

(call-with-input-file "day10_in.txt"
  (lambda (f)
    (let* ([diff (get-differences (all-adapters-order (parse-program f)))]
           [free (cdr (find-free diff))])
      (count-arrangements free))))
