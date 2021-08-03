#!/usr/bin/env guile
!#
(use-modules (ice-9 rdelim)
             (ice-9 ftw)
             (ice-9 futures)
             (srfi srfi-1))
(define get-string-all (@ (rnrs io ports) get-string-all))

(define G (ash 1 30))
(define M (ash 1 20))
(define K (ash 1 10))
(define (filesize size)
  (cond
   ((>= size G)
    (format #f "~,1fGiB" (/ size G)))
   ((>= size M)
    (format #f "~,1fMiB" (/ size M)))
   ((>= size K)
    (format #f "~,1fKiB" (/ size K)))
   (else (format #f "~aB" size))))

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
        (FORMATSTR "~7@a ~9@a ~@a~%"))
    (format #t FORMATSTR "PID" "SWAP" "COMMAND")
    (let lp((next results) (total 0))
      (cond
       ((null? next) (format #t "Total: ~10@a~%" (filesize total)))
       (else
        (let ((item (car next)))
          (format #t FORMATSTR
                  (car item)
                  (filesize (cadr item))
                  (caddr item))
          (lp (cdr next) (+ total (cadr item)))))))))

(main)
