#lang racket

(require racket/string)

(define (parse-byr v)
  (writeln v)
  (if (regexp-match? #rx"^[0-9]+$" v)
      (let ([num (cadr (regexp-match #rx"^0*([1-9][0-9]*)$" v))])
        (and (>= (string->number num 10) 1920) (<= (string->number num 10) 2002)))
      false))

(define (parse-iyr v)
  (if (regexp-match? #rx"^[0-9]+$" v)
      (let ([num (cadr (regexp-match #rx"^0*([1-9][0-9]*)$" v))])
        (and (>= (string->number num 10) 2010) (<= (string->number num 10) 2020)))
      false))

(define (parse-eyr v)
  (if (regexp-match? #rx"^[0-9]+$" v)
      (let ([num (cadr (regexp-match #rx"^0*([1-9][0-9]*)$" v))])
        (and (>= (string->number num 10) 2020) (<= (string->number num 10) 2030)))
      false))

(define (parse-hgt v)
  (if (regexp-match? #rx"[0-9]+(in|cm)" v)
      (let ([parsed (regexp-match #rx"([0-9]+)(in|cm)" v)])
        (cond [(equal? "in" (caddr parsed))
               (and (>= (string->number (cadr parsed) 10) 59)
                    (<= (string->number (cadr parsed) 10) 76))]
              [(equal? "cm" (caddr parsed))
               (and (>= (string->number (cadr parsed) 10) 150)
                    (<= (string->number (cadr parsed) 10) 193))]))
      false))

(define (parse-hcl v)
  (if (regexp-match? #rx"#[0-9a-f]+" v)
      (let ([m (cadr (regexp-match #rx"#([0-9a-f]+)" v))])
        (if (eq? 6 (string-length m))
            true
            false))
      false))

(define (parse-ecl v)
  (if (regexp-match? #rx"amb|blu|brn|gry|grn|hzl|oth" v)
      true
      false))

(define (parse-pid v)
  (if (regexp-match #rx"[0-9]+" v)
      (let ([m (car (regexp-match #rx"[0-9]+" v))])
        (if (eq? 9 (string-length m))
            true
            false))
      false))

;; pass is a vector of 9 elements
(define (add-field-to-passport f pass)
  (match (car f)
    ["byr" (if (eq? 1 (vector-ref pass 1))
               (vector-set! pass 0 0)
               (if (parse-byr (cadr f))
                   (vector-set! pass 1 1)
                   (vector-set! pass 0 0)))]
    ["iyr" (if (eq? 1 (vector-ref pass 2))
               (vector-set! pass 0 0)
               (if (parse-iyr (cadr f))
                   (vector-set! pass 2 1)
                   (vector-set! pass 0 0)))]
    ["eyr" (if (eq? 1 (vector-ref pass 3))
               (vector-set! pass 0 0)
               (if (parse-eyr (cadr f))
                   (vector-set! pass 3 1)
                   (vector-set! pass 0 0)))]
    ["hgt" (if (eq? 1 (vector-ref pass 4))
               (vector-set! pass 0 0)
               (if (parse-hgt (cadr f))
                   (vector-set! pass 4 1)
                   (vector-set! pass 0 0)))]
    ["hcl" (if (eq? 1 (vector-ref pass 5))
               (vector-set! pass 0 0)
               (if (parse-hcl (cadr f))
                   (vector-set! pass 5 1)
                   (vector-set! pass 0 0)))]
    ["ecl" (if (eq? 1 (vector-ref pass 6))
               (vector-set! pass 0 0)
               (if (parse-ecl (cadr f))
                   (vector-set! pass 6 1)
                   (vector-set! pass 0 0)))]
    ["pid" (if (eq? 1 (vector-ref pass 7))
               (vector-set! pass 0 0)
               (if (parse-pid (cadr f))
                   (vector-set! pass 7 1)
                   (vector-set! pass 0 0)))]
    ["cid" (if (eq? 1 (vector-ref pass 8))
               (vector-set! pass 0 0)
               (vector-set! pass 8 1))])
  pass)

(define (parse-input in)
  (let ([passes (regexp-split #rx"  " in)])
    (writeln (length passes))
    (for/list ([pass passes])
      (let ([fields (regexp-split #rx" " pass)])
        (for/list ([f fields])
          (regexp-split #rx":" f))))))

(define (solve1 f)
  (let* ([lines (string-join
                 (for/list ([l (in-lines f)]) l))]
         [parsed (parse-input lines)]
         [passes (for/list ([passport parsed])
                   (let ([newpass (vector 1 0 0 0 0 0 0 0 0)])
                     (foldl add-field-to-passport newpass passport)))])
    (for/fold ([cnt 0])
              ([pass passes])
      (writeln pass)
      (if (or (eq? 0 (vector-ref pass 0))
              (eq? 0 (vector-ref pass 1))
              (eq? 0 (vector-ref pass 2))
              (eq? 0 (vector-ref pass 3))
              (eq? 0 (vector-ref pass 4))
              (eq? 0 (vector-ref pass 5))
              (eq? 0 (vector-ref pass 6))
              (eq? 0 (vector-ref pass 7)))
          (+ cnt 1)
          cnt))))

(call-with-input-file "day4_in.txt"
  (lambda (f)
    (solve1 f)))
