#lang racket

(define (get-next-num nums last_num indx)
  (let ([last_occur (hash-ref nums last_num -1)])
    (if (= last_occur -1)
        0
        (- indx last_occur))))

(define (solve1-helper nums last_num turn_num max_turns)
  (cond [(> turn_num max_turns)
         last_num]
        [else
         (let ([next_num (get-next-num nums last_num (- turn_num 1))])
           (solve1-helper (hash-set nums last_num (- turn_num 1))
                          next_num
                          (+ turn_num 1)
                          max_turns))]))


(define (solve1 nums turns max_turns)
  (let-values ([(nums indx last_turn)
                (for/fold ([new_nums nums]
                           [indx 1]
                           [last_turn (car turns)])
                          ([turn turns])
                  (values (hash-set new_nums turn indx)
                          (+ indx 1)
                          turn))])
    (writeln nums)
    (writeln indx)
    (writeln last_turn)
    (writeln "----")
    (solve1-helper nums last_turn indx max_turns)))

(define (main)
  (let* ([fstring (string-split (string-trim (file->string "day15_in.txt") "\n") ",")]
         [turns (map string->number fstring)])
    (writeln (solve1 (make-immutable-hash) turns 2020))
    (writeln (solve1 (make-immutable-hash) turns 30000000))))
    

(main)
