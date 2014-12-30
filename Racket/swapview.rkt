#!/usr/bin/env racket
#lang racket
(require racket/format)

(define (filesize n)
  (if (< n 1100) (format "~aB" n)
      (letrec (
            [p (floor (inexact->exact (floor (/ (log n) (log 1024)))))]
            [s (~r (/ n (expt 1024 p)) #:precision '(= 1))]
            [unit (string-ref "BKMGT" p)])
        (format "~a~aiB" s unit))))

(define (fmt1 s1 s2 s3)
  (begin
    (map display (list (~a s1  #:width 5 #:align 'right) " "
                       (~a s2  #:width 9 #:align 'right) " " (~a s3)) )
    (newline)))

(define (total n)
  (begin
    (display "Total: ")
    (display (~a n  #:min-width 8))
    (newline)))

(define (string-starts-with? s prefix)
   (equal? (substring s 0 (string-length prefix)) prefix))

(define (getSwapFor pid)
  (with-handlers ([exn:fail:filesystem? (lambda (e) (list pid 0 ""))])
    (begin
      (letrec (
               [cmd (string-trim
                      (string-replace
                       (file->string (format "/proc/~a/cmdline" pid)) "\x0" " "))]
               [swap? (lambda (l) (string-starts-with? l "Swap:"))]
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
          #:key (compose car cdr)
          <)))

(define (main) (begin
  (let ((results (getSwap)))
    (begin
      (fmt1 "PID" "SWAP" "COMMAND")
      (map (curry apply (lambda (pid size cmd) (fmt1 pid (filesize size) cmd))) results))
  (total (filesize (apply + (map (compose car cdr) results)))))))

(main)
