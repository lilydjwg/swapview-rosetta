#lang racket/base
(require
  (only-in racket/file file->string)
  (only-in racket/format ~a ~r)
  (only-in racket/list split-at)
  (only-in racket/math exact-floor)
  (only-in racket/string string-replace))
(provide main)

(module p racket/base
  (require (only-in racket/file file->lines)
           (only-in racket/place place place-channel-put place-channel-get)
           (only-in racket/string string-split string-prefix?))
  (provide getSmaps getSize parallel place-channel-put place-channel-get)
  (define getSmaps (lambda (pid)
                     (file->lines (format "/proc/~a/smaps" pid))))
  (define getSize (lambda (smaps)
                    (* 1024 (apply + (map (lambda (s) (cond [(string-prefix? s "Swap:") (string->number (cadr (string-split s)))]
                                                            [else 0])) smaps)))))
  (define (parallel)
    (place ch
           (define latter (place-channel-get ch))
           (place-channel-put ch (map (lambda (pid) (with-handlers ([exn:fail:filesystem? (lambda (exn) 0)])
                                                      (getSize (getSmaps pid)))) latter)))
    ))

(require 'p)

(define (fmtPid pid) (~a pid  #:width 7 #:align 'right))
(define (filesize n)
  (if (< n 1100) (format "~aB" n)
      (letrec ([p (exact-floor (log (/ n 1100) 1024))]
               [s (~r (/ n (expt 1024 (add1 p))) #:precision '(= 1))]
               [unit (string-ref "KMGT" p)])
        (format "~a~aiB" s unit))))
(define (fmtSize size) (~a size  #:width 9 #:align 'right))
(define (strinit s)
  (let ([s (string-replace s "\x0" " ")]
        [l (string-length s)])
    (if (zero? l) s (substring s 0 (- l 1)))))

(define pl (parallel))
(define pid-list (filter string->number (map path->string (directory-list "/proc"))))
(define len (exact-floor (* (length pid-list) 2/3)))
(define-values (former latter) (split-at pid-list len))
(define-values (size-list result-list)
  (begin
    (place-channel-put pl latter)
    (let ((former (map (lambda (pid) (with-handlers ([exn:fail:filesystem? (lambda (exn) 0)])
                                       (getSize (getSmaps pid)))) former))
          (cmdline-list (map (lambda (pid) (with-handlers ([exn:fail:filesystem? (lambda (exn) "")])
                                             (file->string (format "/proc/~a/cmdline" pid))))
                             pid-list)))
      (define size-list (append former (place-channel-get pl)))
      (values size-list (map (lambda (pid size cmd) (list pid size cmd)) pid-list size-list cmdline-list)))))

(define format-result (map (lambda (result) (list ((compose fmtPid car) result)
                                                  ((compose fmtSize filesize cadr) result)
                                                  ((compose strinit caddr) result)))
                           (sort (filter (lambda (result) (not (zero? (cadr result)))) result-list) #:key cadr <)))

(define (fmt1 s1 s2 s3)
  (begin
    (map display (list s1 " "
                       s2 " "
                       s3))
    (newline)))
(define (total n)
  (begin
    (display "Total: ")
    (display (~a n  #:min-width 10 #:align 'right))
    (newline)))

(define (main)
  (fmt1 (fmtPid "PID") (fmtSize "SWAP") "COMMAND")
  (map (lambda (result) (apply fmt1 result)) format-result)
  (total (filesize (apply + size-list))))