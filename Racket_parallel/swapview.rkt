#lang racket/base
(require
  (only-in racket/list split-at)
  (only-in racket/math exact-floor)
  (submod racket/performance-hint begin-encourage-inline))

(define pid-list (filter string->number (map path->string (directory-list "/proc"))))
(define len (exact-floor (* (length pid-list) 1/2)))
(define-values (former latter) (split-at pid-list len))

(module* p #f
  (require (only-in racket/file file->lines file->string)
           (only-in racket/place place place-channel-put)
           (only-in racket/string string-split string-prefix?))
  (provide getAll parallel)
  (begin-encourage-inline
    (define getSmaps (lambda (pid)
                       (file->lines (format "/proc/~a/smaps" pid))))
    (define getSize (lambda (smaps)
                      (* 1024 (apply + (map (lambda (s) (cond [(string-prefix? s "Swap:") (string->number (cadr (string-split s)))]
                                                              [else 0])) smaps)))))
    (define getCmdline (lambda (pid)
                         (file->string (format "/proc/~a/cmdline" pid))))
    (define getAll (lambda (pid) (with-handlers ([exn:fail:filesystem? (lambda (exn) (list pid 0 ""))])
                                   (list pid (getSize (getSmaps pid)) (getCmdline pid))))))
  (define (parallel)
    (place ch
           (place-channel-put ch (map getAll latter)))
    ))

(module* main #f
  (require (submod ".." p)
           (only-in racket/format ~a ~r)
           (only-in racket/list remove-duplicates)
           racket/match
           (only-in racket/string string-replace)
           (only-in racket/place place-channel-get))
  (begin-encourage-inline
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
        (newline))))

  (define result-list
    (let ((pl (parallel))
          (former (map getAll former)))
      (append former (place-channel-get pl))))

  (define-values (size-list format-result)
    (match (sort (remove-duplicates (filter (lambda (result) (not (zero? (cadr result)))) result-list) string=? #:key car) #:key cadr <)
      ((list (list pid size cmd) ...)
       (values size
               (map (lambda (pid size cmd)
                      (list
                       (fmtPid pid)
                       (fmtSize (filesize size))
                       (strinit cmd)))
                    pid size cmd)))))

  (void
   (fmt1 (fmtPid "PID") (fmtSize "SWAP") "COMMAND")
   (map (lambda (result) (apply fmt1 result)) format-result)
   (total (filesize (apply + size-list)))))