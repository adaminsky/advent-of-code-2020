#lang racket

(define (parse-input in)
  (let ([groups (regexp-split #rx"  " in)])
    (for/list ([group groups])
      (let ([person (regexp-split #rx" " group)])
        (for/list ([p person])
          (string->list p))))))

(define (solve1 parsed)
  (for/fold ([cnt 0])
            ([group parsed])
    (+ cnt (length (remove-duplicates (flatten group))))))

(define (list-intersect ls)
  (cond [(eq? 1 (length ls)) (car ls)]
        [else (set->list (set-intersect
                          (list->set (car ls))
                          (list->set (list-intersect (cdr ls)))))]))

(define (solve2 parsed)
  (for/fold ([cnt 0])
            ([group parsed])
    (+ cnt (length (list-intersect group)))))

(call-with-input-file "day6_in.txt"
  (lambda (f)
    (let* ([lines (string-join (for/list ([l (in-lines f)]) l))]
           [parsed (parse-input lines)])
      (writeln (solve1 parsed))
      (writeln (solve2 parsed)))))
