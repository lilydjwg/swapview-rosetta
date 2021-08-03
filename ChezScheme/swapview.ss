(import (chezscheme))

(define-record process-info (pid swap-usage command-line))

(define (filesize size)
  (let lp ((units '(B KiB MiB GiB TiB))
           (size size))
    (if (and (> size 1100) (not (null? units)))
        (lp (cdr units) (/ size 1024))
        (if (eq? (car units) 'B)
            (format #f "~dB" (inexact->exact size))
            (format #f "~,1f~a" size (car units))))))

(define-syntax try
  (syntax-rules (catch)
    ((_ body (catch catcher))
     (call-with-current-continuation
      (lambda (exit)
        (with-exception-handler
         (lambda (condition)
           (exit catcher))
         (lambda () body)))))))

(define (get-command-line pid)
  (let* [(port (open-input-file (format #f "/proc/~a/cmdline" pid)))
         (raw-commandline (get-string-all port))
         (raw (if (eof-object? raw-commandline) "" raw-commandline))
         (len (- (string-length raw) 1))
         (_ (if (char=? (string-ref raw len) #\nul)
                (string-truncate! raw len)))]
    (list->string
      (map (lambda (x) (if (char=? x #\nul) #\space x))
        (string->list raw)))))

(define (find-number str)
  (list->string
    (filter (lambda (x) (and (char>=? x #\0) (char<=? x #\9)))
      (string->list str))))

(define (getSwapFor pid)
  (try
      (let ((smaps (open-input-file (format #f "/proc/~a/smaps" pid))))
        (let lp ((size 0)
                 (line (get-line smaps)))
          (cond ((eof-object? line)
                 (close-input-port smaps)
                 (and (not (zero? size))
                      (make-process-info
                       pid (* 1024 size) (get-command-line pid))))
                (else
                 (lp (if (string=? (substring line 0 5) "Swap:")
                         (+ size (string->number (find-number line)))
                         size)
                     (get-line smaps))))))
      (catch #f)))

(define (getSwap)
  (sort (lambda (a b) (< (process-info-swap-usage a) (process-info-swap-usage b)))
    (filter (lambda (x) (and x (> (process-info-swap-usage x) 0)))
      (map (lambda (x) (and (string->number x) (getSwapFor x)))
            (directory-list "/proc")))))

(define (main)
  (let ([FORMATSTR "~7@a ~9@a ~@a~%"]
        [total 0.0])
    (format #t FORMATSTR "PID" "SWAP" "COMMAND")
    (for-each
      (lambda (item)
          (begin
            (set! total (+ total (process-info-swap-usage item)))
            (format #t FORMATSTR
              (process-info-pid item)
              (filesize (process-info-swap-usage item))
              (process-info-command-line item))))
        (getSwap))
    (format #t "Total: ~10@a~%" (filesize total))))

(main)
