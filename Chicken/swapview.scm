(use srfi-1 srfi-13 data-structures utils posix)

(include "format/format.scm")
(import format)

(define-syntax while
  (syntax-rules ()
    ((while test body ...)
     (let loop ()
       (if test
           (begin
             body ...
             (loop)))))))

(define (filesize size)
  (let* ((units "KMGT")
         (left (abs size))
         (unit -1))
    (begin
      (while (and (> left 1100) (< unit 3))
        (set! left (/ left 1024))
        (set! unit (+ 1 unit)))
      (if (= unit -1)
        (format #f "~aB" size)
        (begin
          (if (< size 0)
            (set! left (- left)))
          (format #f "~,1f~aiB" left 
            (string-copy units unit (+ 1 unit))))))))

(define (getSwapFor pid)
  (condition-case
    (let* ((port (open-input-file (format #f "/proc/~a/cmdline" pid)))
           (rawcomm (string-map (lambda (x) (if (char=? x #\nul) #\space x))
                                (read-all port)))
           (comm (substring rawcomm 0 (string-length rawcomm)))
           (smaps (open-input-file (format #f "/proc/~a/smaps" pid)))
           (s 0.0))
      (begin
        (let ((line (read-line smaps)))
          (while (not (eof-object? line))
              (if (string=? (substring line 0 5) "Swap:")
                (set! s (+ s (string->number (cadr (reverse (string-split line )))))))
              (set! line (read-line smaps))))
        (list pid (* 1024 s) comm)))
    ((exn file) (list pid 0 ""))))

(define (getSwap)
  (sort
    (filter (lambda (x) (> (list-ref x 1) 0))
      (map (lambda (x) (getSwapFor (string->number x)))
        (filter (lambda (x) (string->number x))
          (directory "/proc"))))
    (lambda (a b) (< (list-ref a 1) (list-ref b 1)))))

(define (main)
  (let* ((results (getSwap))
         (FORMATSTR "~5@a ~9@a ~@a~%")
         (total 0.0))
    (begin
      (format #t FORMATSTR "PID" "SWAP" "COMMAND")
      (map
        (lambda (item)
          (begin
            (set! total (+ total (list-ref item 1)))
            (format #t FORMATSTR
              (list-ref item 0)
              (filesize (list-ref item 1))
              (list-ref item 2))))
        results)
      (format #t "Total: ~8@a~%" (filesize total)))))

(main)
