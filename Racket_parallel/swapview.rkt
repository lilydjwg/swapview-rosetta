
#lang racket
(require racket/format)
(provide main)

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
  (let ((pl1 (place ch
                    (define pid-list (place-channel-get ch))
                    (place-channel-put ch
                                       (map
                                        (lambda (pid) (with-handlers ([exn:fail:filesystem? (lambda (e) (list pid ""))])
                                                        (begin
                                                          (let ([cmd (strinit (string-replace
                                                                               (file->string
                                                                                (format "/proc/~a/cmdline" pid)) "\x0" " "))])
                                                            (list pid cmd)))))
                                        pid-list))))
        (pl2 (place ch
                    (define pid-list (place-channel-get ch))
                    (place-channel-put ch
                                       (map
                                        (lambda (pid) (with-handlers ([exn:fail:filesystem? (lambda (e) (list pid 0))])
                                                        (begin
                                                          (letrec ([swap? (lambda (l) (string-prefix? l "Swap:"))]
                                                                   [getSize (lambda (l) (list-ref (string-split l) 1))]
                                                                   [smaps (filter swap? (file->lines (format "/proc/~a/smaps" pid)))]
                                                                   [size (apply + (map (compose string->number getSize) smaps))])
                                                            (list pid (* size 1000))))))
                                        pid-list)))))
    (let work
      ((cmd-list (place-channel-put/get pl1 pid-list))
       (size-list (place-channel-put/get pl2 pid-list)))
      (cond
        ((or (empty? cmd-list) (empty? size-list)) empty)
        (else
         (cons (list (caar cmd-list) (cadar size-list) (cadar cmd-list)) (work (cdr cmd-list) (cdr size-list))))))))

(define (getSwap)
  (begin
    (sort (filter (lambda (l) (> (list-ref l 1) 0))
                  (getSwapFor
                   (filter string->number
                           (map path->string (directory-list "/proc")))))
          #:key (compose car cdr) <)))

(define (main)
  (let ((results (getSwap)))
    (begin
      (fmt1 "PID" "SWAP" "COMMAND")
      (map (curry apply (lambda (pid size cmd) (fmt1 pid (filesize size) cmd))) results)
      (total (filesize (apply + (map (compose car cdr) results)))))))

;(main)
