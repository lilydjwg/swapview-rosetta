#!/usr/bin/env guile
!#
(gc-disable)
(use-modules (ice-9 rdelim)
             (ice-9 ftw)
             (ice-9 futures)
             (srfi srfi-1))
(define get-string-all (@ (rnrs io ports) get-string-all))

(define G 1073741824.0)
(define M 1048576.0)
(define K 1024.0)
(define (filesize size)
  (call-with-output-string
   (lambda (port)
     (cond
      ((>= size G)
       (display (/ size G) port)
       (display " GiB" port))
      ((>= size M)
       (display (/ size M) port)
       (display  " MiB" port))
      ((>= size K)
       (display (/ size K) port)
       (display " KiB" port))
      (else
       (display size port)
       (display " Bytes" port))))))


(define (getSwapFor pid)
  (define (getswapsize)
    (define smaps (format #f "/proc/~a/smaps" pid))
    (define (getsize l)
      (and (string= (substring/shared l 0 5) "Swap:")
           (list->string (string-fold-right (lambda (x p) (if (char-numeric? x) (cons x p) p)) '() l))))
    (when (not (file-exists? smaps)) (error "File doesn't exist!" smaps))
    (call-with-input-file smaps
      (lambda (port)
        (let lp((line (read-line port)) (total 0))
          (cond
           ((eof-object? line) total)
           ((getsize line)
            => (lambda (n)
                 (lp (read-line port) (+ total (string->number n)))))
           (else (lp (read-line port) total)))))))
  (catch #t
         (lambda ()
           (let ((c (call-with-input-file (format #f "/proc/~a/cmdline" pid) get-string-all))
                 (s (getswapsize)))
             (and s (> s 0) (list pid (ash s 10) c))))
         (lambda (key . value) #f)))

(define (getSwap)
  (sort!
   (filter-map touch (map! (lambda (x) (future (and=> (string->number x) getSwapFor))) (scandir "/proc")))
   (lambda (a b) (< (cadr a) (cadr b)))))

(define (main . args)
  (let ((results (getSwap))
        (FORMATSTR "~5@a ~9@a ~@a~%"))
    (format #t FORMATSTR "PID" "SWAP" "COMMAND")
    (let lp((next results) (total 0))
      (cond
       ((null? next) (format #t "Total: ~8@a~%" (filesize total)))
       (else
        (let ((item (car next)))
          (format #t FORMATSTR
                  (car item)
                  (filesize (cadr item))
                  (caddr item))
          (lp (cdr next) (+ total (cadr item)))))))))

(main)
