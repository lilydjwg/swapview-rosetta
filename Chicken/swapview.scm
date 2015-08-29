(use srfi-1 srfi-13 extras data-structures utils posix)

(include "format/format.scm")
(import format)

(define (filesize size)
  (let lp ((units '(B KiB MiB GiB TiB))
           (size size))
    (if (and (> size 1100) (not (null? units)))
        (lp (cdr units) (/ size 1024))
        (if (eq? (car units) 'B)
            (conc size "B")
            (format #f "~,1f~a" size (car units))))))

(define (getSwapFor pid)
  (condition-case
      (let* ((port (open-input-file (format #f "/proc/~a/cmdline" pid)))
             (rawcomm (string-map (lambda (x) (if (char=? x #\nul) #\space x))
                                  (read-all port)))
             (comm (if (> (string-length rawcomm) 0)
                       (substring rawcomm 0 (- (string-length rawcomm) 1))
                       ""))
             (smaps (open-input-file (format #f "/proc/~a/smaps" pid))))
        (let lp ((size 0)
                 (line (read-line smaps)))
          (if (eof-object? line)
              (list pid (* 1024 size) comm)
              (lp (if (substring=? "Swap:" line)
                      (+ size
                         (string->number
                          (cadr (reverse (string-split line)))))
                      size)
                  (read-line smaps)))))
    ((exn file) (list pid 0 ""))))

(define (getSwap)
  (sort
   (filter-map
    (lambda (file-name)
      (and (string->number file-name)
           (let ((swap (getSwapFor file-name)))
             (and (not (zero? (list-ref swap 1))) swap))))
    (directory "/proc"))
   (lambda (a b) (< (list-ref a 1) (list-ref b 1)))))

(define (main)
  (let* ((results (getSwap))
         (FORMATSTR "~5@a ~9@a ~@a~%")
         (total 0))
    (format #t FORMATSTR "PID" "SWAP" "COMMAND")
    (map
     (lambda (item)
       (set! total (+ total (list-ref item 1)))
       (format #t FORMATSTR
               (list-ref item 0)
               (filesize (list-ref item 1))
               (list-ref item 2)))
     results)
    (format #t "Total: ~8@a~%" (filesize total))))

(main)
