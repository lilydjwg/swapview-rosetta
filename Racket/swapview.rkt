#lang racket/base
(require (only-in racket/format ~a ~r)
         (only-in racket/list filter-map)
         (only-in racket/string string-split string-replace string-prefix?)
         (only-in racket/file file->lines file->string)
         (only-in racket/math exact-floor))
;; Compile with `raco make --no-deps`
(define (filesize n)
  (if (< n 1100) (format "~aB" n)
      (letrec ([p (exact-floor (/ (log (/ n 1100)) (log 1024)))]
               [s (~r (/ n (expt 1024 (add1 p))) #:precision '(= 1))]
               [unit (string-ref "KMGT" p)])
        (format "~a~aiB" s unit))))

(define (fmt1 s1 s2 s3)
  (displayln (~a (~a s1 #:width 7 #:align 'right) " "
                 (~a s2 #:width 9 #:align 'right) " "
                 s3)))

(define (total n)
  (display "Total: ")
  (displayln (~a n  #:min-width 10 #:align 'right)))

(define (strinit s)
  (let ([l (string-length s)])
    (if (zero? l) s (substring s 0 (- l 1)))))

(define (getSwapFor pid)
  (with-handlers ([exn:fail:filesystem? (lambda (e) #f)])
    (let/cc ret
      (letrec ([cmd (strinit (string-replace
                              (file->string
                               (format "/proc/~a/cmdline" pid)) "\x0" " "))]
               [swap? (lambda (l) (string-prefix? l "Swap:"))]
               [getSize (lambda (l) (list-ref (string-split l) 1))]
               [smaps (filter swap? (file->lines (format "/proc/~a/smaps" pid)))]
               [size (apply + (map (compose string->number getSize) smaps))])
        (list pid (* (if (zero? size) (ret #f) size) 1024) cmd)))))

(define (getSwap)
  (sort
   (filter-map getSwapFor
               (filter-map (compose1 string->number path->string) (directory-list "/proc")))
   #:key cadr <))

(define (main)
  (let ((results (getSwap)))
    (fmt1 "PID" "SWAP" "COMMAND")
    (total
     (filesize
      (let loop ((r results) (s 0))
        (cond ((null? r) s)
              (else
               (let ((u (car r)))
                 (fmt1 (car u) (filesize (cadr u)) (caddr u))
                 (loop (cdr r) (+ s (cadr u)))))))))))

(main)
