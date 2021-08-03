(import (chicken sort))
(import (chicken file))
(import (chicken io))
(import (chicken string))
(import (chicken flonum))
(import (chicken format))  ; for sprintf
(import format)            ; for clisp style format from SRFI-28 instead of simple format from chicken.format
(import (srfi 1))          ; for filter-map


;;;; Copy from chicken4-core/utils.scm
;;; Read file as string from given filename or port:
(define (read-all . file)
  (let ([file (optional file ##sys#standard-input)])
    (if (port? file)
	(read-string #f file)
(with-input-from-file file (cut read-string #f) #:binary) ) ) )

(define-record process-info pid swap-usage command-line)


(define (filesize size)
  (let lp ((units '(B KiB MiB GiB TiB))
           (size size))
    (if (and (> size 1100) (not (null? units)))
        (lp (cdr units) (/ size 1024.0))
        (if (eq? (car units) 'B)
            (conc size "B")
            (format #f "~,1f~a" size (car units))))))

(define (get-command-line pid)
  (let* ((cmdline-file (sprintf "/proc/~a/cmdline" pid))
         (raw-commandline (with-input-from-file cmdline-file read-all)))
    (string-translate
     (string-chomp raw-commandline "\x00") #\nul #\space)))

(define (get-process-swap-usage pid)
  (condition-case
      (let ((smaps (open-input-file (sprintf "/proc/~a/smaps" pid))))
        (let lp ((size 0)
                 (line (read-line smaps)))
          (cond ((eof-object? line)
                 (close-input-port smaps)
                 (and (not (zero? size))
                      (make-process-info
                       pid (* 1024 size) (get-command-line pid))))
                (else
                 (lp (if (substring=? "Swap:" line)
                         (+ size
                            (string->number
                             (cadr (reverse (string-split line)))))
                         size)
                     (read-line smaps))))))
    ((exn file) #f)))

(define (get-swapped-processes)
  (sort
   (filter-map
    (lambda (file-name)
      (and (string->number file-name)
           (get-process-swap-usage file-name)))
    (directory "/proc"))
   (lambda (a b)
     (< (process-info-swap-usage a) (process-info-swap-usage b)))))

(define (main)
  (let* ((results (get-swapped-processes))
         (FORMATSTR "~7@a ~9@a ~@a~%")
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
    (format #t "Total: ~10@a~%" (filesize total-swap))))

(main)
