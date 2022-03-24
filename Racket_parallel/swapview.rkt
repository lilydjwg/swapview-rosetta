
#lang racket/base
(require (only-in racket/format ~a ~r)
         (only-in racket/list empty empty?)
         (only-in racket/math exact-floor)
         (only-in racket/string string-split string-prefix?)
         (only-in racket/file file->lines))
(provide main)
;;use "racket -tm compiled/swapview_rkt.zo"

(define (fmt1 s1 s2 s3)
  (begin
    (map display (list s1 " "
                       s2 " "
                       s3))
    (newline)))

(define (fmtPid pid)
  (~a pid  #:width 7 #:align 'right))

(define (filesize n)
  (if (< n 1100) (format "~aB" n)
      (letrec ([p (exact-floor (log (/ n 1100) 1024))]
               [s (~r (/ n (expt 1024 (add1 p))) #:precision '(= 1))]
               [unit (string-ref "KMGT" p)])
        (format "~a~aiB" s unit))))

(define (fmtSize size)
  (~a size #:width 9 #:align 'right))

(define (total n)
  (begin
    (display "Total: ")
    (display (~a n  #:min-width 10 #:align 'right))
    (newline)))

(module p racket/base
  (require (only-in racket/string string-replace)
           (only-in racket/file file->string make-temporary-file)
           (only-in racket/place place))
  (provide pid-list parallel)
  (define pid-list (filter string->number
                           (map path->string (directory-list "/proc"))))
  (define (strinit s)
    (let ([l (string-length s)])
      (if (zero? l) s (substring s 0 (- l 1)))))
  (define temp (make-temporary-file))
  (define (getCmdln)
    (map
     (lambda (pid) (writeln
                    (with-handlers ([exn:fail:filesystem? (lambda (e) "")])
                      (strinit (string-replace
                                (file->string
                                 (format "/proc/~a/cmdline" pid)) "\x0" " "))
                      )
                    (current-output-port)))
     pid-list))
  (define (parallel)
    (place ch
           (with-output-to-file temp getCmdln #:exists 'update))
    temp))

(require 'p)

(define temp (parallel))

(define format-pid (map fmtPid pid-list))

(define (getSwapFor)
  (let ((size-list
         (letrec ([swap? (lambda (l) (string-prefix? l "Swap:"))]
                  [getSize (lambda (l) (cadr (string-split l)))]
                  [getSmaps (lambda (pid)
                              (with-handlers ([exn:fail:filesystem? (lambda (e) empty)])
                                (filter swap? (file->lines (format "/proc/~a/smaps" pid)))))]
                  [loop (lambda (pid-list)
                          (cond
                            [(empty? pid-list) empty]
                            [else (cons (* 1024 (apply + (map (compose string->number getSize) (getSmaps (car pid-list)))))
                                        (loop (cdr pid-list)))]))])
           (loop pid-list))))
    (with-input-from-file temp (lambda () (map
                                           (lambda (pid size)
                                             (list pid size (if (= size 0) 0 ((compose fmtSize filesize) size)) (read (current-input-port))))
                                           format-pid size-list)))))

(define (getSwap)
  (begin
    (sort (filter (lambda (l) (> (cadr l) 0))
                  (getSwapFor))
          #:key cadr <)))

(define results (getSwap))

(define (main)
  (let ((pid-list (map car results))
        (size-list (map cadr results))
        (format-size (map caddr results))
        (cmd-list (map cadddr results)))
    (begin
      (fmt1 (fmtPid "PID") (fmtSize "SWAP") "COMMAND")
      (map (lambda (pid size cmd) (fmt1 pid size cmd)) pid-list format-size cmd-list)
      (total (filesize (apply + size-list))))))
