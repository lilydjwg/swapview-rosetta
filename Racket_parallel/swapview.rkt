
#lang racket
(require racket/format)
(provide main)
;;use "racket -tm compiled/swapview_rkt.zo"
(define (filesize n)
  (if (< n 1024) (format "~aB" n)
      (letrec ([p (exact-floor (log n 1024))]
               [s (~r (/ n (expt 1024 p)) #:precision '(= 1))]
               [unit (string-ref "KMGT" (sub1 p))])
        (format "~a~aiB" s unit))))

(define (fmt1 s1 s2 s3)
  (begin
    (map display (list (~a s1  #:width 7 #:align 'right) " "
                       (~a s2  #:width 9 #:align 'right) " " (~a s3)) )
    (newline)))

(define (total n)
  (begin
    (display "Total: ")
    (display (~a n  #:min-width 10))
    (newline)))

(define (strinit s)
  (let ([l (string-length s)])
    (if (zero? l) s (substring s 0 (- l 1)))))

;(define (string-starts-with? s prefix)
;  (equal? (substring s 0 (string-length prefix)) prefix))

(define (getSwapFor pid-list)
  (letrec ((pl (place ch
                      (define pid-list (place-channel-get ch))
                      (place-channel-put ch
                                         (map
                                          (lambda (pid) (with-handlers ([exn:fail:filesystem? (lambda (e) "")])
                                                          (strinit (string-replace
                                                                    (file->string
                                                                     (format "/proc/~a/cmdline" pid)) "\x0" " "))
                                                          ))
                                          pid-list))))
           (size-list
            (map
             (lambda (pid) (with-handlers ([exn:fail:filesystem? (lambda (e) 0)])
                             (begin
                               (letrec ([swap? (lambda (l) (string-prefix? l "Swap:"))]
                                        [getSize (lambda (l) (list-ref (string-split l) 1))]
                                        [smaps (filter swap? (file->lines (format "/proc/~a/smaps" pid)))]
                                        [size (apply + (map (compose string->number getSize) smaps))])
                                 (* size 1000)))))
             pid-list))
           (cmd-list (place-channel-put/get pl pid-list)))
    (map (lambda (pid size cmd) (list pid size cmd)) pid-list size-list cmd-list)))

(define (getSwap)
  (begin
    (sort (filter (lambda (l) (> (list-ref l 1) 0))
                  (getSwapFor
                   (filter string->number
                           (map path->string (directory-list "/proc")))))
          #:key (compose car cdr) <)))

(define (main)
  (letrec ((results (getSwap))
           (pid-list (map car results))
           (size-list (map cadr results))
           (cmd-list (map caddr results)))
    (begin
      (fmt1 "PID" "SWAP" "COMMAND")
      (map (lambda (pid size cmd) (fmt1 pid (filesize size) cmd)) pid-list size-list cmd-list)
      (total (filesize (apply + size-list))))))

;(main)
