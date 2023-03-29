#lang racket/base
(require (only-in racket/format ~a ~r)
         (submod racket/performance-hint begin-encourage-inline)
         (only-in racket/flonum fllog fl/)
         (only-in racket/fixnum fx->fl fl->fx)
         (only-in racket/string string-split string-replace string-prefix? string-join)
         (only-in racket/file file->string))
(provide main)
;; Compile with `raco make --no-deps`
(begin-encourage-inline
  (define (filesize size)
    (define n (* 1024 size))
    (if (< n 1100) (format "~aB" n)
        (letrec ([p (fl->fx (fl/ (fllog (fl/ (fx->fl n) 1100.0)) (fllog 1024.0)))]
                 [s (~r (/ n (expt 1024 (add1 p))) #:precision '(= 1))]
                 [unit (string-ref "KMGT" p)])
          (format "~a~aiB" s unit))))
  
  (define (fmt1 s1 s2 s3)
    (string-append (~a s1 #:width 7 #:align 'right) " "
                   (~a s2 #:width 9 #:align 'right) " "
                   s3))
  
  (define (total n)
    (string-append "Total: " (~a n #:min-width 10 #:align 'right)))
  
  (define (strinit s)
    (let ((l (string-length s)))
      (if (zero? l) "" (string-replace (substring s 0 (sub1 l)) "\x0" " "))))

  (define (getSwapFor pid)
    (with-handlers ([exn:fail:filesystem? (lambda (e) #f)])
      (let* ([swap? (lambda (l) (string-prefix? l "Swap:"))]
             [getSize (lambda (l) (string->number (list-ref (string-split l) 1)))]
             [smaps (call-with-input-file (format "/proc/~a/smaps" pid) (lambda (in) (for/fold ((r 0)) ((line (in-lines in)))
                                                                                       (if (swap? line) (+ (getSize line) r) r))))])
        (if (zero? smaps) #f (list pid smaps (file->string (format "/proc/~a/cmdline" pid))))))))
  
(define (main)
    
  (define main-thd (current-thread))
  
  (void (thread (lambda () (let loop ((pid-list (directory-list "/proc")))
                             (cond ((null? pid-list) (thread-send main-thd #f))
                                   ((string->number (path->string (car pid-list)))
                                    (define v (getSwapFor (car pid-list)))
                                    (cond (v (thread-send main-thd v)))
                                    (loop (cdr pid-list)))
                                   (else (loop (cdr pid-list))))))))

  (define (insert i l (r null))
    (cond ((null? l) (reverse (cons i r)))
          ((>= (car i) (caar l))
           (insert i (cdr l) (cons (car l) r)))
          (else (append (reverse r) (cons i l)))))
  
  (let loop ((t 0) (s (list (cons 0 (fmt1 "PID" "SWAP" "COMMAND")))))
    (define v (thread-receive))
    (cond (v
           (loop (+ t (cadr v)) (insert (cons (cadr v) (fmt1 (car v) (filesize (cadr v)) (strinit (caddr v)))) s)))
          (else (display (string-append (string-join (map cdr s) "\n" #:after-last "\n") (total (filesize t)) "\n")))))
  
  )

(main)
