#lang racket

;; Each line consists of a single character action followed by an
;; integer value. The input data is converted to a list of tuples
;; with the first entry as the action and the second as the integer.
(define (parse-program f)
  (for/list ([l (in-lines f)])
    (cons (substring l 0 1)
          (string->number (substring l 1)))))

(struct direction (theta))
(struct location (x y))

(define (update-dir dir rl deg)
  (cond [(equal? rl "R")
         (struct-copy direction dir [theta (- (direction-theta dir)
                                              (degrees->radians deg))])]
        [(equal? rl "L")
         (struct-copy direction dir [theta (+ (direction-theta dir)
                                              (degrees->radians deg))])]))

(define (rotate waypt rad)
  (let ([old_x (location-x waypt)]
        [old_y (location-y waypt)])
    (struct-copy location waypt
                 [x (- (* old_x (cos rad))
                       (* old_y (sin rad)))]
                 [y (+ (* old_x (sin rad))
                       (* old_y (cos rad)))])))

(define (update-waypt waypt action amt)
  (cond [(equal? action "N")
        (struct-copy location waypt [y (+ (location-y waypt) amt)])]
        [(equal? action "S")
         (struct-copy location waypt [y (- (location-y waypt) amt)])]
        [(equal? action "E")
         (struct-copy location waypt [x (+ (location-x waypt) amt)])]
        [(equal? action "W")
         (struct-copy location waypt [x (- (location-x waypt) amt)])]
        [(equal? action "R")
         (rotate waypt (degrees->radians (- 0 amt)))]
        [(equal? action "L")
         (rotate waypt (degrees->radians amt))]))

(define (update-loc loc dir action amt)
  (cond [(equal? action "N")
        (struct-copy location loc [y (+ (location-y loc) amt)])]
        [(equal? action "S")
         (struct-copy location loc [y (- (location-y loc) amt)])]
        [(equal? action "E")
         (struct-copy location loc [x (+ (location-x loc) amt)])]
        [(equal? action "W")
         (struct-copy location loc [x (- (location-x loc) amt)])]
        [(equal? action "F")
         (struct-copy location loc
                      [x (+ (location-x loc)
                            (* amt (cos (direction-theta dir))))]
                      [y (+ (location-y loc)
                            (* amt (sin (direction-theta dir))))])]))

(define (update-loc-2 loc waypt amt)
  (struct-copy location loc
               [x (+ (location-x loc) (* amt (location-x waypt)))]
               [y (+ (location-y loc) (* amt (location-y waypt)))]))

(define (run-actions-2 loc waypt actions)
  (cond [(empty? actions) loc]
        [(equal? (caar actions) "F")
         (run-actions-2 (update-loc-2 loc waypt (cdar actions))
                        waypt
                        (cdr actions))]
        [else
         (run-actions-2 loc
                        (update-waypt waypt (caar actions) (cdar actions))
                        (cdr actions))]))

(define (run-actions loc dir actions)
  (cond [(empty? actions) loc]
        [(or (equal? (caar actions) "R") (equal? (caar actions) "L"))
         (run-actions loc
                      (update-dir dir (caar actions) (cdar actions))
                      (cdr actions))]
        [else
         (run-actions (update-loc loc dir (caar actions) (cdar actions))
                      dir
                      (cdr actions))]))

(call-with-input-file "day12_in.txt"
  (lambda (f)
    (let* ([parsed (parse-program f)]
           [result1 (run-actions (location 0 0)
                                 (direction 0)
                                 parsed)]
           [result2 (run-actions-2 (location 0 0)
                                   (location 10 1)
                                   parsed)])
      ;; Write output for part 1
      (writeln (+ (abs (location-x result1))
                  (abs (location-y result1))))

      ;; Write output for part 2
      (writeln (+ (abs (location-x result2))
                  (abs (location-y result2)))))))
