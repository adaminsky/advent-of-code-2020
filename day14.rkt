#lang racket

(define (file->list f)
  (for/list ([l (in-lines f)])
    l))

(struct mask (ones zeros))

;; Constructs a mask struct from the mask string.
;;
;; mask0: int representing zero part of mask being constructed.
;; mask1: int representing one part of mask being constructed.
;; mask_list: list representation of the mask.
;; index: offset of the most significant digit of the mask. Ranges
;;        from 35 to 0.
(define (parse-mask-helper mask0 mask1 mask_list index)
  (cond [(empty? mask_list)
         (mask mask1 mask0)]
        [(equal? (car mask_list) #\X)
         (parse-mask-helper (+ mask0 (expt 2 index))
                            mask1
                            (cdr mask_list)
                            (- index 1))]
        [(equal? (car mask_list) #\1)
         (parse-mask-helper (+ mask0 (expt 2 index))
                            (+ mask1 (expt 2 index))
                            (cdr mask_list)
                            (- index 1))]
        [(equal? (car mask_list) #\0)
         (parse-mask-helper mask0
                            mask1
                            (cdr mask_list)
                            (- index 1))]))

(define (apply-mask-helper-v2 loc mask_list index)
  (cond [(empty? mask_list)
         (list loc)]
        [(equal? (car mask_list) #\X)
         (append (apply-mask-helper-v2 (bitwise-xor loc (arithmetic-shift 1 index))
                                  (cdr mask_list)
                                  (- index 1))
               (apply-mask-helper-v2 loc (cdr mask_list) (- index 1)))]
        [(equal? (car mask_list) #\1)
         (apply-mask-helper-v2 (bitwise-ior loc (arithmetic-shift 1 index))
                            (cdr mask_list)
                            (- index 1))]
        [(equal? (car mask_list) #\0)
         (apply-mask-helper-v2 loc
                            (cdr mask_list)
                            (- index 1))]))

(define (apply-mask loc mask_str)
  (apply-mask-helper-v2 loc (string->list mask_str) 35))

(define (parse-mask mask_str)
  (parse-mask-helper 0 0 (string->list mask_str) 35))

(define (parse-store store_line)
  (let* ([matches (regexp-match #rx"mem\\[([0-9]+)\\] = ([0-9]+)" store_line)]
         [index (string->number (cadr matches))]
         [value (string->number (caddr matches))])
    (cons index value)))

(define (run-program mem mask lines)
  (cond [(empty? lines)
         mem]
        [(regexp-match? #rx"mask" (car lines))
         (run-program mem (parse-mask (caddr (string-split (car lines)))) (cdr lines))]
        [else
         (let* ([ind_val (parse-store (car lines))]
                [val_masked (bitwise-and (mask-zeros mask)
                                         (bitwise-ior (mask-ones mask)
                                                      (cdr ind_val)))])
           (run-program (hash-set mem (car ind_val) val_masked)
                        mask (cdr lines)))]))

(define (run-program-v2 mem mask lines)
  (cond [(empty? lines)
         mem]
        [(regexp-match? #rx"mask" (car lines))
         (run-program-v2 mem (caddr (string-split (car lines))) (cdr lines))]
        [else
         (let* ([ind_val (parse-store (car lines))]
                [addrs (apply-mask (car ind_val) mask)]
                [mem-update (for/fold ([mem_acc mem])
                                      ([loc addrs])
                              (hash-set mem_acc loc (cdr ind_val)))])
           (run-program-v2 mem-update mask (cdr lines)))]))

(define (sum-memory mem)
  (foldl + 0
         (map cdr (hash->list mem))))

(call-with-input-file "day14_in.txt"
  (lambda (f)
    (let ([flist (file->list f)])
      (writeln (sum-memory (run-program (make-immutable-hash) (mask 0 0) flist)))
      (writeln (sum-memory (run-program-v2 (make-immutable-hash) "" flist))))))
