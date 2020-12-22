#lang racket

(define (process-input in)
  (for/list ([l (in-lines in)])
    (let ([row (string->number (string-replace
                                (string-replace
                                 (substring l 0 7) "F" "0")
                                "B" "1")
                               2)]
          [col (string->number (string-replace
                                (string-replace
                                 (substring l 7 10) "L" "0")
                                "R" "1")
                               2)])
      (+ (* 8 row) col))))

(call-with-input-file "day5_in.txt"
  (lambda (f)
    (apply max (process-input f))))

(call-with-input-file "day5_in.txt"
  (lambda (f)
    (for/fold ([indx 0])
              ([lst (sort (process-input f) <)])
      (if (not (eq? (+ 70 indx) lst))
          (begin (writeln (- lst 1)) (+ indx 2))
          (+ indx 1)))))
