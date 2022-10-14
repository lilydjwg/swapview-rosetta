#lang racket/base
(require (only-in racket/format ~a ~r)
         (only-in racket/string string-split string-replace string-prefix?)
         (only-in racket/file file->lines file->string)
         (only-in racket/math exact-floor)
         (only-in racket/function curry))
;; Compile with `raco make --no-deps`
(define (filesize n)
  (if (< n 1100) (format "~aB" n)
      (letrec ([p (exact-floor (/ (log (/ n 1100)) (log 1024)))]
               [s (~r (/ n (expt 1024 (add1 p))) #:precision '(= 1))]
               [unit (string-ref "KMGT" p)])
        (format "~a~aiB" s unit))))

(define (fmt1 s1 s2 s3)
  (begin
    (map display (list (~a s1  #:width 7 #:align 'right) " "
                       (~a s2  #:width 9 #:align 'right) " " (~a s3)) )
    (newline)))

(define (total n)
  (begin
    (display "Total: ")
    (displayln (~a n  #:min-width 10 #:align 'right)))))

(define (strinit s)
  (let ([l (string-length s)])
    (if (zero? l) s (substring s 0 (- l 1)))))

(define (getSwapFor pid)
  (with-handlers ([exn:fail:filesystem? (lambda (e) (list pid 0 ""))])
    (begin
      (letrec ([cmd (strinit (string-replace
                              (file->string
                               (format "/proc/~a/cmdline" pid)) "\x0" " "))]
               [swap? (lambda (l) (string-prefix? l "Swap:"))]
               [getSize (lambda (l) (list-ref (string-split l) 1))]
               [smaps (filter swap? (file->lines (format "/proc/~a/smaps" pid)))]
               [size (apply + (map (compose string->number getSize) smaps))])
        (list pid (* size 1024) cmd)))))

(define (getSwap)
  (begin
    (sort (filter (lambda (l) (> (list-ref l 1) 0))
                  (map getSwapFor
                       (filter string->number
                               (map path->string (directory-list "/proc")))))
          #:key (compose car cdr) <)))

(define (main)
  (let ((results (getSwap)))
    (begin
      (fmt1 "PID" "SWAP" "COMMAND")
      (map (curry apply (lambda (pid size cmd) (fmt1 pid (filesize size) cmd))) results)
      (total (filesize (apply + (map (compose car cdr) results)))))))

(main)
