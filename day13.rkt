#lang racket

(define (file->list f)
  (for/list ([l (in-lines f)])
    l))

(struct in_data (depart_time active_buses))

(define (input->in_data in)
  (let* ([in_split (file->list in)]
         [raw_depart_time (car in_split)]
         [raw_all_buses (string-split (cadr in_split) ",")]
         [bus_indices (map cons
                           (range (length raw_all_buses))
                           raw_all_buses)])
    (in_data (string->number raw_depart_time)
             (map (lambda (n) (cons (car n) (string->number (cdr n))))
                  (filter (lambda (n) (not (equal? (cdr n) "x")))
                          bus_indices)))))

(define (find-first-bus in_data)
  (argmin cdr
          (map (lambda (n)
                 (cons n
                       (- (cdr n) (modulo (in_data-depart_time in_data) (cdr n)))))
               (in_data-active_buses in_data))))

;; Implementation of a sieve for the Chinese remainder theorem.
;;
;; args:
;;   t: the candidate solution
;;   add: offset between possible solutions
;;   buses: the set of bus constraints where each element contains the
;           bus offset and id.
(define (find-first-t t add buses)
  (if (eq? (modulo (- (cdar buses) (caar buses)) (cdar buses))
           (modulo t (cdar buses)))
      (if (empty? (cdr buses))
          t
          (find-first-t t
                        (* add (cdar buses))
                        (cdr buses)))
      (find-first-t (+ t add) add buses)))

(define (find-first buses)
  (find-first-t (modulo (- (cdar buses)
                           (caar buses))
                        (cdar buses))
                (cdar buses)
                (cdr buses)))

(call-with-input-file "day13_in.txt"
  (lambda (f)
    (let* ([parsed (input->in_data f)]
           [best_bus (find-first-bus parsed)]
           [bus_id (cdar best_bus)]
           [depart_wait (cdr best_bus)])
      (writeln (* bus_id depart_wait))
      (writeln (find-first (sort (in_data-active_buses parsed)
                                 (lambda (a b) (> (cdr a) (cdr b))))))))) 
