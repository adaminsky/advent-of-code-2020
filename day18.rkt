#lang racket

(define ns (make-base-namespace))

(define (file->lines f)
  (for/list ([l (in-lines f)])
    l))

(define (extract-subexpr sub stack)
  (cond [(equal? (car stack) "(")
         (cons sub (cdr stack))]
        [else
         (extract-subexpr (cons (car stack) sub)
                          (cdr stack))]))

(define (parser stack toks)
  (cond [(empty? toks)
         (car (extract-subexpr '() stack))]
        [(or (equal? (car toks) " ") (equal? (car toks) ""))
         (parser stack (cdr toks))]
        [(equal? (car toks) ")")
         (parser (extract-subexpr '() stack) (cdr toks))]
        [(equal? (car toks) "+")
         (parser (cons '+ stack) (cdr toks))]
        [(equal? (car toks) "*")
         (parser (cons '* stack) (cdr toks))]
        [(equal? (car toks) "(")
         (parser (cons "(" stack) (cdr toks))]
        [else
         (parser (cons (string->number (car toks)) stack) (cdr toks))]))

(define (my-eval1-helper tmp op expr)
  (let ([res (apply (eval op ns) (list tmp (my-eval1 (car expr))))])
    (if (empty? (cdr expr))
        res
        (my-eval1-helper res (cadr expr) (cddr expr)))))

(define (my-eval1 expr)
  (cond [(not (list? expr))
         expr]
        [(and (list? (car expr)) (> (length expr) 1))
         (my-eval1-helper (my-eval1 (car expr)) (cadr expr) (cddr expr))]
        [(list? (car expr))
         (my-eval1 (car expr))]
        [(> (length expr) 1)
         (my-eval1-helper (car expr) (cadr expr) (cddr expr))]
        [else
         (car expr)]))

(define (solve1 str)
  (my-eval1 (parser (list "(") (string-split str ""))))

(define (solve2 str)
  (my-eval2 (parser (list "(") (string-split str ""))))

(define (my-eval2-helper tmp op expr)
  (if (equal? op '+)
      (let ([res (apply (eval op ns) (list tmp (my-eval2 (car expr))))])
        (if (empty? (cdr expr))
            res
            (my-eval2-helper res (cadr expr) (cddr expr))))
      (let ([res (apply (eval op ns) (list tmp (my-eval2 expr)))])
        res)))

(define (my-eval2 expr)
  (cond [(not (list? expr))
         expr]
        [(and (list? (car expr)) (> (length expr) 1))
         (my-eval2-helper (my-eval2 (car expr)) (cadr expr) (cddr expr))]
        [(list? (car expr))
         (my-eval2 (car expr))]
        [(> (length expr) 1)
         (my-eval2-helper (car expr) (cadr expr) (cddr expr))]
        [else
         (car expr)]))

(call-with-input-file "day18_in.txt"
  (lambda (f)
    (let ([lines (file->lines f)])
      (writeln (for/fold ([sum 0])
                         ([l lines])
                 (+ sum (solve1 l))))
      (writeln (for/fold ([sum 0])
                         ([l lines])
                 (+ sum (solve2 l)))))))
