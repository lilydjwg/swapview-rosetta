#lang racket/base

(module shared racket/base
  (require (only-in racket/flonum fllog fl/ fl+ flexpt flfloor ->fl)
           (submod racket/performance-hint begin-encourage-inline)
           (only-in racket/file file->string)
           (only-in racket/string string-split string-prefix? string-replace)
           (only-in racket/format ~a ~r))
  (provide insert in-pqueue path-sequence->swap-size-pqueue output)

  (begin-encourage-inline
    ;;A priority queue represented as a vector
    (define (pqueue) (vector))
    (define (in-pqueue vec) (in-vector vec))
    (define (insert vec v (key car))
      (define (search vec v (key car))
        (let loop ((start 0) (end (vector-length vec)))
          (cond ((= start end) start)
                (else (define h (floor (/ (+ start end) 2)))
                      (cond ((< (key v) (key (vector-ref vec h))) (loop start h))
                            (else (if (= 1 (- end start)) end (loop h end))))))))

      (let ((new (make-vector (add1 (vector-length vec))))
            (i (search vec v key)))
        (vector-copy! new 0 vec 0 i)
        (vector-set! new i v)
        (vector-copy! new (add1 i) vec i)
        new))

    ;;bytes and integers
    (define (digits? bstr)
      (let/cc break
        (for ((b (in-bytes bstr)))
          (cond ((or (< b 48) (> b 57)) (break #f))))
        #t))
    (define (digits->integer bstr)
      (for/fold ((c 0)) ((b (in-bytes bstr)))
        (+ (* c 10) (- b 48))))

    ;; basic formatters
    (define (format-pid pid) (~a pid  #:width 7 #:align 'right))
    (define (filesize size)
      (define n (* 1024 size))
      (if (< n 1100) (format "~aB" n)
          (let* ([fn (->fl n)]
                 [p (flfloor (fl/ (fllog (fl/ fn 1100.0)) (fllog 1024.0)))]
                 [s (~r (fl/ fn (flexpt 1024.0 (fl+ 1.0 p))) #:precision '(= 1))]
                 [unit (string-ref "KMGT" (inexact->exact p))])
            (format "~a~aiB" s unit))))
    (define (format-size size) (~a size  #:width 9 #:align 'right))
    (define (resolve-cmdline s)
      (let ([l (string-length s)])
        (if (zero? l) s (string-replace (substring s 0 (- l 1)) "\x0" " "))))
    (define (fmt1 s1 s2 s3)
      (string-append s1 " " s2 " " s3))
    (define (total n)
      (string-append "Total: " (~a n #:min-width 10 #:align 'right)))

    ;; swap-size-pqueue: (pqueueof (cons/c exact-positive-integer? string?) ...)
    ;; the constructor
    (define (path-sequence->swap-size-pqueue seq)
      (define (get-smaps pid) (format "/proc/~a/smaps" pid))
      
      (define (get-size input)
        (for/fold ((r 0)) ((l (in-bytes-lines input)))
          (define matched (regexp-match #px#"^Swap:\\s+([0-9]+)" l))
          (cond (matched (+ r (digits->integer (cadr matched)))) (else r))))
      
      (define (get-cmdline pid) (file->string (format "/proc/~a/cmdline" pid)))
      
      (define (resolve-pid pid)
        (with-handlers ([exn:fail:filesystem? (lambda (exn) #f)])
          (let ((v (call-with-input-file (get-smaps pid) get-size)))
            (if (zero? v) #f (cons v (fmt1 (format-pid pid) (format-size (filesize v)) (resolve-cmdline (get-cmdline pid))))))))
      
      (for/fold ((r (pqueue))) ((p seq))
        (let ((v (and (digits? (path->bytes p)) (resolve-pid p))))
          (cond (v (insert r v)) (else r)))))
    ;; the logger
    (define (output pqueue)
      (displayln (fmt1 (format-pid "PID") (format-size "SWAP") "COMMAND"))
      (displayln
       (total
        (filesize
         (for/fold ((t 0)) ((v (in-pqueue pqueue)))
           (displayln (cdr v))
           (+ t (car v)))))))))

(module* helper racket/base
  (require (submod ".." shared)
           (only-in racket/async-channel make-async-channel async-channel-put)
           (only-in racket/place place place-channel-put))
  (provide main)

  (define (main)
    (define (make-place _)
      (let ((pl (place ch
                  (place-channel-put ch (path-sequence->swap-size-pqueue (in-producer sync #f ch))))))
        (cons pl pl)))
    (define (make-thread)
      (define ch (make-async-channel))
      (cons ch
            (thread
             (lambda ()
               (async-channel-put ch (path-sequence->swap-size-pqueue (in-producer thread-receive #f)))))))

    ;; channel-pair: (cons <in-channel> <out-channel>)
    (define (send chp v)
      (define out (cdr chp))
      ((if (thread? out) thread-send place-channel-put) out v))
    (define (recv chp)
      (sync (car chp)))

    (define (append-pqueue v1 . vl)
      (for/fold ((r v1)) ((o (in-list vl)))
        (for/fold ((r r)) ((val (in-pqueue o)))
          (insert r val))))
    
    (define place-number 2)
    
    (let ((chp-lst (cons (make-thread) (build-list place-number make-place))))
      (for ((v (in-list (directory-list "/proc"))) (chp (in-cycle (in-list chp-lst))))
        (send chp v))
      (map (lambda (chp) (send chp #f)) chp-lst)
      (output (apply append-pqueue (map (lambda (chp) (recv chp)) chp-lst))))))

(module* main racket/base
  (require (submod ".." helper))

  (main))
