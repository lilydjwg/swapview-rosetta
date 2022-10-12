#lang racket/base

(module* shared #f
  (require (only-in racket/list split-at)
           (only-in racket/math exact-floor)
           (submod racket/performance-hint begin-encourage-inline)
           (only-in racket/file file->lines file->string)
           (only-in racket/string string-split string-prefix? string-replace)
           (only-in racket/format ~a ~r))
  (provide former latter getAll fmtPid fmtSize filesize)

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
    (define (fmtPid pid) (~a pid  #:width 7 #:align 'right))
    (define (filesize n)
      (if (< n 1100) (format "~aB" n)
          (let ([p (exact-floor (log (/ n 1100) 1024))])
            (define s (~r (/ n (expt 1024 (add1 p))) #:precision '(= 1)))
            (define unit (string-ref "KMGT" p))
            (format "~a~aiB" s unit))))
    (define (fmtSize size) (~a size  #:width 9 #:align 'right))
    (define (resolveCmdline s)
      (let ([s (string-replace s "\x0" " ")]
            [l (string-length s)])
        (if (zero? l) s (substring s 0 (- l 1)))))
    (define getAll (lambda (pid)
                     (with-handlers ([exn:fail:filesystem? (lambda (exn) #f)])
                       (let/cc ret
                         (list
                          (let ((v (getSize (getSmaps pid))))
                            (if (zero? v) (ret #f) (cons v (fmtSize (filesize v)))))
                          (fmtPid pid)
                          (resolveCmdline (getCmdline pid)))))))))

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
                           (place-channel-put ch (map getAll '#,latter)))))
               (append (map getAll '#,former) (place-channel-get pl))))))

  (define (getResult) (parallel)))

(module* main #f
  (require (only-in (submod ".." helper) getResult)
           (only-in (submod ".." shared) fmtPid fmtSize filesize)
           racket/match
           (only-in racket/format ~a)
           (submod racket/performance-hint begin-encourage-inline))

  (begin-encourage-inline
    (define (fmt1 s1 s2 s3)
      (displayln (~a s1 " " s2 " " s3)))
    (define (total n)
      (displayln (~a "Total: " n #:min-width 10 #:align 'right))))

  (define result-list (getResult))

  (define-values (size-list format-result)
    (match (sort (filter values result-list) #:key caar <)
      ((list (list (cons size format-size) pid cmd) ...)
       (values size
               (map (lambda (pid format-size cmd)
                      (list pid format-size cmd))
                    pid format-size cmd)))))

  (void
   (fmt1 (fmtPid "PID") (fmtSize "SWAP") "COMMAND")
   (map (lambda (result) (apply fmt1 result)) format-result)
   (total (filesize (apply + size-list)))))