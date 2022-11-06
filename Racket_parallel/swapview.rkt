#lang racket/base

(module* shared #f
  (require (only-in racket/math exact-floor)
           (submod racket/performance-hint begin-encourage-inline)
           (only-in racket/file file->lines file->string)
           (only-in racket/string string-split string-prefix? string-replace)
           (only-in racket/format ~a ~r))
  (provide getAll fmtPid fmtSize filesize)

  (begin-encourage-inline
    (define getSmaps (lambda (pid) (format "/proc/~a/smaps" pid)))
    (define swap? (lambda (line) (string-prefix? line "Swap:")))
    (define getSize (lambda (input)
                      (let loop ((line (read-line input)) (result 0))
                        (cond ((eof-object? line) result)
                              ((swap? line) (loop (read-line input) (+ result (string->number (cadr (string-split line))))))
                              (else (loop (read-line input) result))))))
    (define getCmdline (lambda (pid) (file->string (format "/proc/~a/cmdline" pid))))
    (define (fmtPid pid) (~a pid  #:width 7 #:align 'right))
    (define (filesize size)
      (define n (* 1024 size))
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
                          (let ((v (call-with-input-file (getSmaps pid) getSize)))
                            (if (zero? v) (ret #f) (cons v (fmtSize (filesize v)))))
                          (fmtPid pid)
                          (resolveCmdline (getCmdline pid)))))))))

(module* helper #f
  (require (for-syntax racket/base
                       (only-in racket/list split-at filter-map)
                       (only-in racket/math exact-floor))
           (only-in (submod ".." shared) getAll)
           (only-in racket/list filter-map)
           (only-in racket/place place place-channel-put))
  (provide getResult)

  (begin-for-syntax
    (define pid-list (filter-map (compose1 string->number path->string) (directory-list "/proc")))
    (define len (exact-floor (* (length pid-list) 1/2)))
    (define-values (former latter) (split-at pid-list len)))

  (define-syntax (parallel stx)
    #`(let ((pl (place ch (place-channel-put ch (filter-map getAll '#,latter)))))
        (let ((f (filter-map getAll '#,former))
              (l (sync pl)))
          (append f l))))

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
      (display "Total: ")
      (displayln (~a n #:min-width 10 #:align 'right))))

  (define result-list (getResult))

  (define-values (size-list format-result)
    (match (sort result-list #:key caar <)
      ((list (list (cons size format-size) pid cmd) ...)
       (values size
               (map (lambda (pid format-size cmd)
                      (list pid format-size cmd))
                    pid format-size cmd)))))

  (void
   (fmt1 (fmtPid "PID") (fmtSize "SWAP") "COMMAND")
   (map (lambda (result) (apply fmt1 result)) format-result)
   (total (filesize (apply + size-list)))))
