#lang racket

(require racket/set)

(define (get-tiles input)
  (let* ([raw_tiles (string-split input "\n\n")]
         [tiles (map (lambda (x)
                       (cdr (regexp-match #px"Tile (\\d+):\n(.*)" x)))
                     raw_tiles)])
    (for/fold ([res (list)])
              ([tile tiles])
      (cons (list (string->number (car tile))
                  (map string->list (string-split (cadr tile) "\n")))
            res))))

(define (get-border-set tile)
  (let* ([trans (apply map list (cadr tile))]
         [rev (reverse (cadr tile))]
         [revtrans (reverse trans)])
    (list (caadr tile)
          (car rev)
          (car trans)
          (car revtrans))))

(define (make-border-set tiles)
  (for/fold ([res (list)])
            ([tile tiles])
    (append (get-border-set tile) res)))

(define (count-matches borders edge)
  (length (filter (lambda (x)
                    (or (equal? x edge)
                        (equal? (reverse x) edge)))
                  borders)))

(define (border-matches borders tiles)
  (map (lambda (x)
         (let ([loc_borders (get-border-set x)])
           (cons (car x) (- (foldl (lambda (n res)
                                  (+ res (count-matches borders n)))
                                0
                                loc_borders)
                            4))))
       tiles))

(define (main)
  (let* ([fstring (file->string "day20_in.txt")]
         [tiles (get-tiles fstring)]
         [borders (make-border-set tiles)]
         [border_matches (border-matches borders tiles)])
    (apply * (map
              car
              (filter (lambda (x) (equal? 2 (cdr x)))
                      border_matches)))))

(main)
