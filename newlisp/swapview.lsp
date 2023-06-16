#!/usr/bin/newlisp

(define item-format "%7s %9s %s")
(define total-format "Total: %10s")

(define (file-size size)
  (let ((units "KMGT")
        (left (abs size))
        (unit -1))
    (while (and (> left 1100) (< unit 3))
           (setq left (div left 1024))
           (++ unit))
    (if (= unit -1)
      (format "%dB" size)
      (begin
        (if (< size 0)
          (setq left (- left)))
        (format "%.1f%siB" left (units unit))))))

(define (get-pid-dir pid dir)
  (append "/proc/" pid "/" dir))

(context 'with-open)
(define-macro (with-open:with-open fname mode fhandle lst-if lst-else)
              (if (set fhandle (eval (expand '(open fname mode) 'fname 'mode)))
                (begin
                  (if lst-if
                    (dolist (x lst-if)
                      (eval x)))
                  (close (eval fhandle)))
                (begin
                  (if lst-else
                    (dolist (x lst-else)
                      (eval x))))))

(context MAIN) 

(define (read-proc-file pid fname)
  (let ((fname (get-pid-dir pid fname))
        (fcontent ""))
    (with-open fname "r" f
               ((while (read f buf 4096)
                       (extend fcontent (replace "\000" buf " ")))))
    (let (out (trim fcontent))
      (if (> (length out) 0) out nil))))

(define (get-swap-for pid)
  (let ((comm (read-proc-file pid "cmdline"))
        (fs (read-proc-file pid "smaps"))
        (off 0)
        (sum 0)
        (found '()))
    (if fs
      (while (setf found (regex {\nSwap:\s+(\d+)} fs 0 off))
             (setq off (+ (found 1) (found 2)))
             (setq sum (+ sum (int (found 3))))))
    (list pid (* sum 1024) comm)))

(define (get-swap)
  (let (ret '())
    (dolist (x (directory "/proc/" "[0-9]+"))
      (let (s (get-swap-for x))
        (if (> (s 1) 0)
          (begin
            (extend ret (list s)))))
      (sort ret (fn (s1 s2) (< (s1 1) (s2 1)))))))

(define (main)
  (let ((results (get-swap))
        (sum 0))
    (println (format item-format "PID" "SWAP" "COMMAND"))
    (dolist (s results)
      (println (format item-format (s 0) (file-size (s 1)) (s 2)))
      (setq sum (+ sum (s 1))))
    (println (format total-format (file-size sum)))))

(main)
(exit)
