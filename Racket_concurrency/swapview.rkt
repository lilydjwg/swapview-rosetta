#lang racket/base
(require (only-in racket/format ~a ~r)
         (submod racket/performance-hint begin-encourage-inline)
         (only-in racket/list filter-map)
         (only-in racket/string string-split string-replace string-prefix?)
         (only-in racket/file file->string)
         (only-in racket/async-channel make-async-channel async-channel-put)
         (only-in racket/math exact-floor))
(provide main)
;; Compile with `raco make --no-deps`
(begin-encourage-inline
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
    (let ((l (string-length s)))
      (if (zero? l) "" (string-replace (substring s 0 (sub1 l)) "\x0" " "))))

  (define (getSwapFor pid)
    (with-handlers ([exn:fail:filesystem? (lambda (e) #f)])
      (let/cc ret
        (let* ([swap? (lambda (l) (string-prefix? l "Swap:"))]
               [getSize (lambda (l) (list-ref (string-split l) 1))]
               [smaps (call-with-input-file (format "/proc/~a/smaps" pid) (lambda (in) (for/fold ((r 0)) ((line (in-lines in)))
                                                                                         (if (swap? line) (+ (getSize line) r) r))))])
          (list pid (* (if (zero? smaps) (ret #f) smaps) 1024) (file->string (format "/proc/~a/cmdline" pid))))))))
  
(define (main)
  (fmt1 "PID" "SWAP" "COMMAND")
  
  (define channel (make-async-channel))
  
  (define thd (thread (lambda () (let loop ((pid-list (sort (filter-map (compose string->number path->string) (directory-list "/proc")) <)))
                                    (cond ((null? pid-list) (async-channel-put channel #f))
                                          (else (async-channel-put channel (getSwapFor (car pid-list)))
                                                (loop (cdr pid-list))))))))
  
  (let loop ((t 0))
    (define v (sync channel))
    (cond (v
           (fmt1 (car v) (filesize (cadr v)) (strinit (caddr v)))
           (loop (+ t (cadr v))))
          (else (total (filesize t)))))
  
  )

(main)
