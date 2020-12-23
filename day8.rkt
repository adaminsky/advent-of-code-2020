#lang racket

(define (parse-program p)
  (list->vector (regexp-match* #rx"([a-z]+) (\\+|\\-)([0-9]+)"
                               p
                               #:match-select cdr)))

(define (find-loop prog visited acc pc)
  (if (< pc (vector-length prog))
      (let ([instr (car (vector-ref prog pc))]
            [sign (bytes->string/utf-8 (cadr (vector-ref prog pc)))]
            [num (bytes->string/utf-8 (caddr (vector-ref prog pc)))])
        (cond [(set-member? visited pc) acc]
              [(equal? #"nop" instr)
               (find-loop prog (set-add visited pc) acc (+ pc 1))]
              [(equal? #"acc" instr)
               (find-loop prog (set-add visited pc)
                          (if (equal? "+" sign)
                              (+ acc (string->number num))
                              (- acc (string->number num)))
                          (+ pc 1))]
              [(equal? #"jmp" instr)
               (find-loop prog (set-add visited pc) acc
                          (if (equal? "+" sign)
                              (+ pc (string->number num))
                              (- pc (string->number num))))]))
      false))

(define (remove-loop prog)
  (for ([loc (in-range (vector-length prog))])
    (cond [(equal? #"jmp" (car (vector-ref prog loc)))
           (begin (vector-set! prog loc (cons #"nop" (cdr (vector-ref prog loc))))
                  (if (not (find-loop prog (set) 0 0))
                      (writeln
                       (find-loop (vector-append prog #((#"jmp" #"+" #"0")))
                                  (set)
                                  0
                                  0))
                      '())
                  (vector-set! prog loc (cons #"jmp" (cdr (vector-ref prog loc)))))]
          [(equal? #"nop" (car (vector-ref prog loc)))
           (begin (vector-set! prog loc (cons #"jmp" (cdr (vector-ref prog loc))))
                  (if (not (find-loop prog (set) 0 0))
                      (writeln
                       (find-loop (vector-append prog #((#"jmp" #"+" #"0")))
                                  (set)
                                  0
                                  0))
                      '())
                  (vector-set! prog loc (cons #"nop" (cdr (vector-ref prog loc)))))])))

(call-with-input-file "day8_in.txt"
  (lambda (f)
    ;; Uncomment for part 1
    ;; (find-loop (parse-program f) (set) 0 0)
    (remove-loop (parse-program f))))
