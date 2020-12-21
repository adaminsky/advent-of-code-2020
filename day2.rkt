#lang racket

(define (parse-input f)
  (for/list ([l (in-lines f)])
    (tokenize-input l)))

;; index 0: min
;; index 1: max
;; index 2: character
;; index 3: passward
(define (tokenize-input in)
  (let* ([split1 (string-split in ":")]
         [split2 (string-split (list-ref split1 0))]
         [split3 (string-split (list-ref split2 0) "-")])
    (vector (string->number (list-ref split3 0))
            (string->number (list-ref split3 1))
            (string-ref (list-ref split2 1) 0)
            (string-trim (list-ref split1 1)))))

(define (validate-1 password)
  (let* ([pass-list (string->list (vector-ref password 3))]
         [found (map (lambda (x) (eqv? x (vector-ref password 2))) pass-list)]
         [sum (foldl (lambda (a res) (if a (+ res 1) res)) 0 found)])
    (and (>= sum (vector-ref password 0))
         (<= sum (vector-ref password 1)))))

(define (validate-2 password)
  (let* ([pass-list (string->list (vector-ref password 3))]
         [low (- (vector-ref password 0) 1)]
         [high (- (vector-ref password 1) 1)])
    (if (< high (length pass-list))
        (xor (equal? (vector-ref password 2)
                     (list-ref pass-list low))
             (equal? (vector-ref password 2)
                     (list-ref pass-list high)))
        false)))

(call-with-input-file "day2_in.txt"
  (lambda (f)
    (let ([input (parse-input f)])
      (display (foldl
                (lambda (a res) (if (validate-1 a) (+ res 1) res))
                0
                input))
      (display " ")
      (display (foldl
                (lambda (a res) (if (validate-2 a) (+ res 1) res))
                0
                input)))))
