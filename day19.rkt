#lang racket

(define (parse-rhs lhs)
  (cond [(empty? lhs) '(())]
        [(regexp-match? #rx"\".*\"" (car lhs))
         (let ([char (string-ref (cadr (regexp-match #rx"\"(.*)\"" (car lhs))) 0)]
               [rest (parse-rhs (cdr lhs))])
           (cons (cons char (car rest)) (cdr rest)))]
        [(equal? "|" (car lhs))
         (cons '() (parse-rhs (cdr lhs)))]
        [else
         (let ([num (string->number (car lhs))]
               [rest (parse-rhs (cdr lhs))])
           (cons (cons num (car rest)) (cdr rest)))]))

(define (parse-derivation line)
  (let ([rule (regexp-match #px"(\\d+): (.*)$" line)])
    (list (string->number (cadr rule))
          (parse-rhs (string-split (caddr rule))))))

(define (construct-grammar-table lines)
  (for/fold ([gram (make-immutable-hash)])
            ([line lines])
    (let ([parsed (parse-derivation line)])
      (hash-set gram (car parsed) (cadr parsed)))))

(define (match-rule grammar rhs input)
  (cond [(empty? rhs)
         (if (empty? input)
             true
             false)]
        [(list? (car rhs))
         (match-rule-or grammar
                        (car rhs)
                        (cdr rhs)
                        '()
                        input)]
        [else
         (match-rule-seq grammar rhs '() input)]))

(define (match-rule-seq grammar rhs rhs_rest input)
  (cond [(empty? rhs)
         (match-rule grammar rhs_rest input)]
        [(empty? input)
         false]
        [(list? (car rhs))
         (match-rule-or grammar (car rhs) (cdr rhs) rhs_rest input)]
        [(number? (car rhs))
         (match-rule-seq grammar
                         (hash-ref grammar (car rhs))
                         (append (cdr rhs) rhs_rest)
                         input)]
        [(equal? (car input) (car rhs))
         (match-rule-seq grammar (cdr rhs) rhs_rest (cdr input))]
        [else false]))

(define (match-rule-or grammar rhs rhs_rest cont input)
  (if (empty? rhs_rest)
      (match-rule-seq grammar rhs cont input)
      (or (match-rule-seq grammar rhs cont input)
          (match-rule-seq grammar rhs_rest cont input))))

(call-with-input-file "day19_in.txt"
  (lambda (f)
    (let* ([fstr (file->string "day19_test.txt")]
           [grammar (construct-grammar-table
                     (string-split (cadr (regexp-match #rx"(.*?\n)\n" fstr)) "\n"))]
           [start (hash-ref grammar 0)]
           [messages (string-split (cadr (regexp-match #rx"\n\n(.*)$" fstr)) "\n")])
      (for/fold ([cnt 0])
                ([message messages])
        (if (match-rule grammar start (string->list message))
            (+ cnt 1)
            cnt)))))
