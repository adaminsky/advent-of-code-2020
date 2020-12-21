#lang racket

(define (input->list f)
  (for/list ([l (in-lines f)])
    (string->number l)))

;; Simple brute force solution which finds every pair of numbers in the list.
(call-with-input-file "day1_in.txt"
  (lambda (f)
    (let* ([v (list->vector (input->list f))]
           [len (vector-length v)])
      (for* ([i (in-range len)]
             [j (in-range len)]
            #:unless (or (< j i) (eqv? i j)))
        (if (eqv? (+ (vector-ref v i) (vector-ref v j)) 2020)
            (printf "~a\n" (* (vector-ref v i) (vector-ref v j)))
            '())))))


(call-with-input-file "day1_in.txt"
  (lambda (f)
    (let* ([v (list->vector (input->list f))]
           [len (vector-length v)])
      (for* ([i (in-range len)]
             [j (in-range len)]
             [k (in-range len)]
            #:unless (or (<= k j) (<= j i)))
        (if (eqv? (+ (vector-ref v i) (vector-ref v j) (vector-ref v k)) 2020)
            (printf "~a\n"
                    (* (vector-ref v i) (vector-ref v j) (vector-ref v k)))
            '())))))
