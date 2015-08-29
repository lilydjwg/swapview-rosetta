(use srfi-1 extras data-structures utils posix)

(include "format/format.scm")
(import format)

(define-record process-info pid swap-usage command-line)

(define (filesize size)
  (let lp ((units '(B KiB MiB GiB TiB))
           (size size))
    (if (and (> size 1100) (not (null? units)))
        (lp (cdr units) (/ size 1024))
        (if (eq? (car units) 'B)
            (conc size "B")
            (format #f "~,1f~a" size (car units))))))

(define (get-command-line pid)
  (let* ((cmdline-file (format #f "/proc/~a/cmdline" pid))
         (raw-commandline (with-input-from-file cmdline-file read-all)))
    (string-translate
     (string-chomp raw-commandline "\x00") #\nul #\space)))

(define (get-process-swap-usage pid)
  (condition-case
      (let* ((command-line (get-command-line pid))
             (smaps (open-input-file (format #f "/proc/~a/smaps" pid))))
        (let lp ((size 0)
                 (line (read-line smaps)))
          (cond ((eof-object? line)
                 (close-input-port smaps)
                 (make-process-info pid (* 1024 size) command-line))
                (else
                 (lp (if (substring=? "Swap:" line)
                         (+ size
                            (string->number
                             (cadr (reverse (string-split line)))))
                         size)
                     (read-line smaps))))))
    ((exn file) (make-process-info pid 0 ""))))

(define (get-swapped-processes)
  (sort
   (filter-map
    (lambda (file-name)
      (and (string->number file-name)
           (let ((info (get-process-swap-usage file-name)))
             (and (not (zero? (process-info-swap-usage info))) info))))
    (directory "/proc"))
   (lambda (a b)
     (< (process-info-swap-usage a) (process-info-swap-usage b)))))

(define (main)
  (let* ((results (get-swapped-processes))
         (FORMATSTR "~5@a ~9@a ~@a~%")
         (total-swap 0))
    (format #t FORMATSTR "PID" "SWAP" "COMMAND")
    (map
     (lambda (swapped-process-info)
       (set! total-swap
         (+ total-swap (process-info-swap-usage swapped-process-info)))
       (format #t FORMATSTR
               (process-info-pid swapped-process-info)
               (filesize (process-info-swap-usage swapped-process-info))
               (process-info-command-line swapped-process-info)))
     results)
    (format #t "Total: ~8@a~%" (filesize total-swap))))

(main)
