
#lang racket/base
(require racket/format)
(require racket/place)
(require racket/list)
(require racket/math)
(require racket/string)
(require racket/file)
(provide main)
;;use "racket -tm compiled/swapview_rkt.zo"

(define pid-list (filter string->number
                         (map path->string (directory-list "/proc"))))

(define results (getSwap))

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

(define (strinit s)
  (let ([l (string-length s)])
    (if (zero? l) s (substring s 0 (- l 1)))))

(define (getSwapFor)
  (define pl
    (place ch
           (place-channel-put ch
                              (map
                               (lambda (pid) (with-handlers ([exn:fail:filesystem? (lambda (e) "")])
                                               (strinit (string-replace
                                                         (file->string
                                                          (format "/proc/~a/cmdline" pid)) "\x0" " "))
                                               ))
                               pid-list))))
  (let ((format-pid (map fmtPid pid-list))
        (size-list
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
           (loop pid-list)))
        (cmd-list (place-channel-get pl)))
    (map (lambda (pid size cmd) (list pid size cmd)) format-pid size-list cmd-list)))

(define (getSwap)
  (begin
    (sort (filter (lambda (l) (> (cadr l) 0))
                  (getSwapFor))
          #:key cadr <)))

(define (main)
  (let ((pid-list (map car results))
        (size-list (map cadr results))
        (cmd-list (map caddr results)))
    (begin
      (fmt1 (fmtPid "PID") (fmtSize "SWAP") "COMMAND")
      (map (lambda (pid size cmd) (fmt1 pid ((compose fmtSize filesize) size) cmd)) pid-list size-list cmd-list)
      (total (filesize (apply + size-list))))))
