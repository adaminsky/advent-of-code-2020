#lang racket

;; Converts the input file to a 2d vector where the first dimension represents
;; the line number and the second dimension is the location within the line.
(define (input->2dvector f)
  (for/vector ([l (in-lines f)])
    (for/vector ([c (string->list l)])
      c)))

;; world is a 2d vector of chars representing the world with '#' representing a
;; tree. Returns true if location (x, y) in world is a tree.
(define (tree-count world x y)
  (let ([width (vector-length (vector-ref world 0))])
    (match (vector-ref (vector-ref world y)
                       (modulo x width))
      [#\# 1]
      [_ 0])))

(define (solve-slope world right down)
  (let* ([height (vector-length world)]
         [width (* right height)])
    (for/fold ([num-trees 0])
              ([x (in-range 0 width right)]
               [y (in-range 0 height down)])
      (+ num-trees (tree-count world x y)))))

(call-with-input-file "day3_in.txt"
  (lambda (f)
    (let ([world (input->2dvector f)])
      (* (solve-slope world 1 1)
         (solve-slope world 3 1)
         (solve-slope world 5 1)
         (solve-slope world 7 1)
         (solve-slope world 1 2)))))
