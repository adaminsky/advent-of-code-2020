#lang racket

(define (parse-conditions filestr)
  (let* ([conds (string-split (cadr (regexp-match #rx"(.*?\n)\n" filestr)) "\n")])
    (for/fold ([pconds '()])
              ([line conds])
      (cons (map string->number
                 (cdr (regexp-match #px".*?: (\\d+)-(\\d+) or (\\d+)-(\\d+)" line)))
            pconds))))

(define (parse-tickets filestr)
  (map (lambda (ln) (map string->number (string-split ln ",")))
       (string-split (cadr (regexp-match #rx"nearby tickets:\n(.*$)" filestr)) "\n")))

(define (parse-my-ticket filestr)
  (car (map (lambda (ln) (map string->number (string-split ln ",")))
            (string-split (cadr (regexp-match #rx"your ticket:\n(.*\n)\n" filestr)) "\n"))))

(define (valid? conds num)
  (for/or ([ranges conds])
    (or (and (>= num (car ranges))
             (<= num (cadr ranges)))
        (and (>= num (caddr ranges))
             (<= num (cadddr ranges))))))

(define (valid-label? ranges vals)
  (for/and ([val vals])
    (valid? (list ranges) val)))

(define (valid-ticket? conds ticket)
  (for/and ([num ticket])
    (valid? conds num)))

(define (transpose ls)
  (apply map list ls))

(define (solve1 conds tickets)
  (for/fold ([error 0])
            ([ticket tickets])
    (+ error (foldl + 0 (filter (lambda (n) (not (valid? conds n))) ticket)))))

(define (assign-labels conds possible mapping n)
  (if (< n (length possible))
      (let* ([next (caar (filter (lambda (n) (= (length n) 1)) possible))]
             [cond_indx (index-of conds next)]
             [val_indx (index-of possible (list next))]
             [new_possible (map (lambda (n)
                                  (filter (lambda (m) (not (equal? next m))) n))
                                possible)])
        (assign-labels conds new_possible (cons (cons cond_indx val_indx)
                                                mapping) (+ n 1)))
      mapping))

(define (solve2 conds vals_by_label my_ticket)
  (let* ([possible (reverse (for/fold ([pos '()])
                                      ([vals vals_by_label])
                              (cons (filter (lambda (n) (valid-label? n vals))
                                            conds)
                                    pos)))]
         [field_val_map (make-immutable-hash (assign-labels conds possible '() 0))])
    (writeln field_val_map)
    (writeln (hash-ref field_val_map 0))
    ;; fields 0-5 all start with "departure"
    (*
     (list-ref my_ticket (hash-ref field_val_map 0))
     (list-ref my_ticket (hash-ref field_val_map 1))
     (list-ref my_ticket (hash-ref field_val_map 2))
     (list-ref my_ticket (hash-ref field_val_map 3))
     (list-ref my_ticket (hash-ref field_val_map 4))
     (list-ref my_ticket (hash-ref field_val_map 5)))))

(define (main)
  (let* ([fstring (file->string "day16_in.txt")]
         [tickets (parse-tickets fstring)]
         [my_ticket (parse-my-ticket fstring)]
         [conds (reverse (parse-conditions fstring))]
         [vals_by_label (transpose (filter (lambda (n) (valid-ticket? conds n)) tickets))])
    (writeln conds)
    (writeln (solve1 conds tickets))
    (writeln (solve2 conds vals_by_label my_ticket))))

(main)
