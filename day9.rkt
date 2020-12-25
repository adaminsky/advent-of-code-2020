#lang racket

(define (parse-program f)
  (for/list ([l (in-lines f)])
    (string->number l)))

;; Brute force solution
;; n is the starting index for the window of 25 numbers
;; nums is a list of numbers
(define (get-sums nums n)
  (let ([ints (list->vector (take (drop nums n) 25))])
    (for*/set ([i (in-range 25)]
               [j (in-range 25)]
               #:when (not (eq? i j)))
      (+ (vector-ref ints i) (vector-ref ints j)))))

;; Return the sum of numbers from index pos to pos+len-1
(define (get-sum nums pos len)
  (apply + (take (drop nums pos) len)))

(call-with-input-file "day9_in.txt"
  (lambda (f)
    (let ([nums (parse-program f)]
          [target 248131121])
      (for ([i (in-range 25 (length nums))])
        (if (set-member? (get-sums nums (- i 25)) (list-ref nums i))
            '()
            (writeln (list-ref nums i))))
      (for* ([len (in-range 2 (length nums))]
             [indx (in-range (length nums))]
             #:unless (or (> (+ indx len) (length nums))
                          (> (get-sum nums indx len) target)))
        (if (eq? (get-sum nums indx len) target)
            (writeln (+ (apply min (take (drop nums indx) len))
                        (apply max (take (drop nums indx) len))))
            '())))))
