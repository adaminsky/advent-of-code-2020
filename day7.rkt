#lang racket

(define (add-bag bags desc)
  (let ([color (regexp-match #rx"^[a-z]+ [a-z]+" desc)]
        [inside (regexp-match*
                 #rx"([0-9]+) ([a-z]+ [a-z]+) (?=bags\\.|bag\\.|bags\\, |bag\\, )"
                 desc
                 #:match-select cdr)])
    (hash-set! bags (car color) inside)))

(define (contains-shiny-gold bags ls)
  (for/fold ([cnt 0])
            ([bag ls])
    (let* ([color (cadr bag)]
           [inside (hash-ref bags color)])
      (cond [(equal? color "shiny gold") (+ cnt 1)]
            [(eq? 0 (length inside)) cnt]
            [else (+ cnt (contains-shiny-gold bags inside))]))))

(define (solve1 bags)
  (for/fold ([cnt 0])
            ([(color inside) (in-hash bags)])
    (+ cnt (if (< 0 (contains-shiny-gold bags inside))
               1
               0))))

(define (solve2 bags inside)
  (cond [(eq? 0 (length inside)) 0]
        [(eq? 1 (length inside))
         (+ (string->number (caar inside))
            (* (string->number (caar inside))
            (solve2 bags (hash-ref bags (cadr (car inside))))))]
        [else (+ (solve2 bags (cons (car inside) '()))
                 (solve2 bags (cdr inside)))]))

(call-with-input-file "day7_in.txt"
  (lambda (f)
    (let ([bags (make-hash)])
      (for ([l (in-lines f)])
        (add-bag bags l))
      (solve1 bags)
      (- (solve2 bags '(("1" "shiny gold"))) 1))))
