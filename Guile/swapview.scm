#!/usr/bin/env guile
!#
(use-modules (ice-9 format))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 ftw))

(define (filesize size)
  (let* ((units "KMGT") 
         (left (abs size))
         (unit -1))
    (begin
      (while
        (and (> left 1100) (< unit 3))
        (begin
          (set! left (/ left 1024))
          (set! unit (+ 1 unit))))
      (if (= unit -1)
        (format #f "~aB" size)
        (begin
          (if (< size 0) 
            (set! left (- left)))
          (format #f "~,1f~aiB" left (string-copy units unit (+ 1 unit)))
        )
      )
    )
  )
)

(define (getSwapFor pid)
  (catch #t
    (lambda ()
      (let* ((port (open-input-file (format #f "/proc/~a/cmdline" pid)))
             (comm (string-trim-right (string-map (lambda (x) (if (char=? x #\nul) #\sp x))
                               (read-string port))))
             (smaps (open-input-file (format #f "/proc/~a/smaps" pid)))
             (s 0.0)
             )
        (begin
          (while (char-ready? smaps)
            (let ((line (read-line smaps)))
              (if (eof-object? line) (break) 
                (if (> (string-length line) 5)
                  (if (string=? (substring line 0 5) "Swap:")
                    (set! s (+ s (string->number (cadr (reverse (string-split line #\sp))))))
                  )
                )
              )
            )
          )
          (list pid (* 1024 s) comm)
        )
      )
    )
    (lambda (key . value) (list pid 0 ""))
  )
)

(define (getSwap)
  (sort
    (filter (lambda (x) (> (list-ref x 1) 0)) 
      (map (lambda (x) (getSwapFor (string->number x)))
        (scandir "/proc" (lambda (x) (string->number x))))
    )
    (lambda (a b) (< (list-ref a 1) (list-ref b 1)))
  )
)

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
              (list-ref item 2) 
            )
          )
        )
        results
      )
      (format #t "Total: ~8@a~%" (filesize total))
    )
  )
)

(main)
