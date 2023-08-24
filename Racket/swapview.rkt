#lang racket/base
(require (submod "../Racket_parallel/swapview.rkt" shared))
(provide main)
(define (main)
  (output (path-sequence->swap-size-pqueue (in-list (directory-list "/proc")))))

(main)
