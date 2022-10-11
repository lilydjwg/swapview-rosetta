#lang racket/base

(module* shared #f
  (require (only-in racket/list split-at)
           (only-in racket/math exact-floor)
           (submod racket/performance-hint begin-encourage-inline)
           (only-in racket/file file->lines file->string)
           (only-in racket/string string-split string-prefix?))
  (provide former latter getAll)

  (define pid-list (filter string->number (map path->string (directory-list "/proc"))))
  (define len (exact-floor (* (length pid-list) 1/2)))
  (define-values (former latter) (split-at pid-list len))
  (begin-encourage-inline
    (define getSmaps (lambda (pid)
                       (file->lines (format "/proc/~a/smaps" pid))))
    (define getSize (lambda (smaps)
                      (* 1024 (apply + (map (lambda (s) (cond [(string-prefix? s "Swap:") (string->number (cadr (string-split s)))]
                                                              [else 0])) smaps)))))
    (define getCmdline (lambda (pid)
                         (file->string (format "/proc/~a/cmdline" pid))))
    (define getAll (lambda (pid) (with-handlers ([exn:fail:filesystem? (lambda (exn) #f)])
                                   (let/cc ret (list pid (let ((v (getSize (getSmaps pid)))) (if (zero? v) (ret #f) v)) (getCmdline pid))))))))

(module* helper #f
  (require (for-syntax racket/base
                       (only-in (submod ".." shared) former latter))
           (only-in (submod ".." shared) getAll)
           (only-in racket/place place place-channel-put place-channel-get))
  (provide getResult)

  (define-syntax (parallel stx)
    (syntax-case stx ()
      ((_) #`(let ((pl
                    (place ch
                           (place-channel-put ch (map getAll (list #,@latter))))))
               (append (map getAll (list #,@former)) (place-channel-get pl))))))

  (define (getResult) (parallel)))

(module* main #f
  (require (submod ".." helper)
           (only-in racket/format ~a ~r)
           racket/match
           (only-in racket/math exact-floor)
           (only-in racket/string string-replace)
           (submod racket/performance-hint begin-encourage-inline))

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

  (define result-list (getResult))

  (define-values (size-list format-result)
    (match (sort (filter values result-list) #:key cadr <)
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