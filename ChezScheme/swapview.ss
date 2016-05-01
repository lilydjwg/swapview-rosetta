(import (chezscheme))

(define-syntax while
  (syntax-rules ()
    ((_ pred b1 ...)
     (let loop () (when pred b1 ... (loop))))))

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
            (format #f "~,1f~aiB" left (substring units unit (+ 1 unit))))))))

(define (string-split str ch)
  (let ((len (string-length str)))
    (letrec
      ((split
        (lambda (a b)
          (cond
            ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
              ((char=? ch (string-ref str b)) (if (= a b)
                (split (+ 1 a) (+ 1 b))
                  (cons (substring str a b) (split b b))))
                (else (split a (+ 1 b)))))))
                  (split 0 0))))

(define-syntax try
  (syntax-rules (catch)
    ((_ body (catch catcher))
     (call-with-current-continuation
      (lambda (exit)
        (with-exception-handler
         (lambda (condition)
           (exit catcher))
         (lambda () body)))))))

(define (getSwapFor pid)
    (try
      (let* ((port (open-input-file (format #f "/proc/~a/cmdline" pid)))
            (rawinput (get-string-some port))
            (rawcomm (list->string (map (lambda (x) (if (char=? x #\nul) #\space x))
                     (string->list (if (eof-object? rawinput) "" rawinput)))))
            (comm (if (> (string-length rawcomm) 0)
                    (substring rawcomm 0 (- (string-length rawcomm) 1))
                    ""))
            (smaps (open-input-file (format #f "/proc/~a/smaps" pid)))
            (s 0.0))
          (call/cc (lambda (brk)
            (while (char-ready? smaps)
              (let ((line (get-line smaps)))
                (if (eof-object? line) (brk)
                  (if (> (string-length line) 5)
                    (if (string=? (substring line 0 5) "Swap:")
                      (set! s (+ s (string->number (cadr (reverse (string-split line #\space)))))))))))
          ))
          (list pid (* 1024 s) comm))
      (catch (list pid 0 ""))))

(define (getSwap)
  (sort (lambda (a b) (< (list-ref a 1) (list-ref b 1)))
    (filter (lambda (x) (> (list-ref x 1) 0))
      (map (lambda (x) (getSwapFor x))
        (filter (lambda (x) (number? x))
          (map (lambda (x) (string->number x))
            (directory-list "/proc")))))))

(define (main)
  (let ([FORMATSTR "~5@a ~9@a ~@a~%"]
        [total 0.0])
    (display (format #f FORMATSTR "PID" "SWAP" "COMMAND"))
    (for-each (lambda (item) 
              (begin
                (set! total (+ total (list-ref item 1)))
                (format #t FORMATSTR
                    (list-ref item 0)
                    (filesize (list-ref item 1))
                    (list-ref item 2))
                ))
        (getSwap))
    (format #t "Total: ~8@a~%" (filesize total))))

(main)
