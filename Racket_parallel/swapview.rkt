#lang racket/base
(require
  (for-syntax racket/base)
  (only-in racket/file file->string)
  (only-in racket/format ~a ~r)
  (only-in racket/place dynamic-place*)
  (only-in racket/runtime-path define-runtime-path)
  (only-in racket/string string-replace)
  (except-in (file "place.rkt") parallel))
(provide main)
(define-runtime-path path (string->path "place.rkt"))

(define (fmtPid pid) (~a pid  #:width 7 #:align 'right))
(define (filesize n)
  (if (< n 1100) (format "~aB" n)
      (letrec ([p (exact-floor (/ (log (/ n 1100)) (log 1024)))]
               [s (~r (/ n (expt 1024 (add1 p))) #:precision '(= 1))]
               [unit (string-ref "KMGT" p)])
        (format "~a~aiB" s unit))))
(define (fmtSize size) (~a size  #:width 9 #:align 'right))
(define (strinit s)
  (let ([s (string-replace s "\x0" " ")]
        [l (string-length s)])
    (if (zero? l) s (substring s 0 (- l 1)))))
(define (port->list in)
  (cond [(byte-ready? in) (read in)]
        [else (port->list in)]))

(define-values (in out) (make-pipe))
(define-values (size-list result-list)
  (let-values (((pl i o e) (dynamic-place* path 'parallel #:in (current-input-port) #:out out #:err (current-error-port)))
               ((former) (getSize (getSmaps former)))
               ((cmdline-list) (map (lambda (pid) (with-handlers ([exn:fail:filesystem? (lambda (exn) "")])
                                                    (file->string (format "/proc/~a/cmdline" pid))))
                                    pid-list)))
    (define size-list (append former (port->list in)))
    (values size-list (map (lambda (pid size cmd) (list pid size cmd)) pid-list size-list cmdline-list))))

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