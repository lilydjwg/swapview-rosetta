#lang racket/base
(require (submod "../Racket_parallel/swapview.rkt" shared))
(provide main)
(define (main)
  (define tbl (make-table))
  (parameterize ((current-directory "/proc"))
    (for ((pid (in-directory #f (lambda (_) #f))))
      (define pair (resolve-pid pid))
      (cond (pair (table-update! tbl (car pair) (cdr pair))))))
  (print-table tbl))

(main)
