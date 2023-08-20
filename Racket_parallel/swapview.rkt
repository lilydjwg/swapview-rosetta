#lang racket/base

(module shared racket/base
  (require (only-in racket/flonum fllog fl/ fl+ flexpt flfloor ->fl)
           (submod racket/performance-hint begin-encourage-inline)
           (only-in racket/file file->string)
           (only-in racket/string string-split string-prefix? string-replace)
           (only-in racket/format ~a ~r))
  (provide insert path-sequence->result-vector output)

  (begin-encourage-inline
    ;;A binary tree represented as a vector
    (define (half s e) (floor (/ (+ s e) 2)))
    (define (search vec v (key car))
      (let loop ((start 0) (end (vector-length vec)))
        (cond ((= start end) start)
              (else (define h (half start end))
                    (cond ((< (key v) (key (vector-ref vec h))) (loop start h))
                          (else (if (= 1 (- end start)) end (loop h end))))))))
    (define (insert vec v (key car))
      (let ((new (make-vector (add1 (vector-length vec))))
            (i (search vec v key)))
        (vector-copy! new 0 vec 0 i)
        (vector-set! new i v)
        (vector-copy! new (add1 i) vec i)
        new))

    ;;bytes and integers
    (define (digit? b) (and (>= b 48) (<= b 57)))
    (define (digits? bstr)
      (let/cc break
        (for ((byte (in-bytes bstr)))
          (cond ((not (digit? byte)) (break #f))))
        #t))
    (define (swap-line->integer bstr)
      (for/fold ((r 0)) ((byte (in-bytes bstr 5 (- (bytes-length bstr) 3))))
        (if (digit? byte) (+ (* r 10) (- byte 48)) r)))

    ;;readers and formatters
    (define (get-smaps pid) (format "/proc/~a/smaps" pid))
    (define (swap? line) (and (>= (bytes-length line) 5) (bytes=? (subbytes line 0 5) #"Swap:")))
    (define (get-size input)
      (for/fold ((c 0)) ((l (in-bytes-lines input)))
        (if (swap? l) (+ c (swap-line->integer l)) c)))
    (define (get-cmdline pid) (file->string (format "/proc/~a/cmdline" pid)))
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
    ;; result-pair : (cons/c exact-positive-integer? string?)
    (define (resolve-pid pid)
      (with-handlers ([exn:fail:filesystem? (lambda (exn) #f)])
        (let ((v (call-with-input-file (get-smaps pid) get-size)))
          (if (zero? v) #f (cons v (fmt1 (format-pid pid) (format-size (filesize v)) (resolve-cmdline (get-cmdline pid))))))))
    ;; result-vector : (vectorof <result-pair>)
    (define (path-sequence->result-vector seq)
      (for/fold ((r (vector))) ((p seq))
        (let ((v (and (digits? (path->bytes p)) (resolve-pid p))))
          (cond (v (insert r v)) (else r)))))
    (define (output result-vector)
      (displayln (fmt1 (format-pid "PID") (format-size "SWAP") "COMMAND"))
      (displayln
       (total
        (filesize
         (for/fold ((t 0)) ((v (in-vector result-vector)))
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
                  (place-channel-put ch (path-sequence->result-vector (in-producer sync #f ch))))))
        (cons pl pl)))
    (define (make-thread)
      (define ch (make-async-channel))
      (cons ch
            (thread
             (lambda ()
               (async-channel-put ch (path-sequence->result-vector (in-producer thread-receive #f)))))))
    (define (send ch v)
      ((if (thread? ch) thread-send place-channel-put) ch v))
    (define (append-result-vector v1 . vl)
      (for/fold ((r v1)) ((o (in-list vl)))
        (for/fold ((r r)) ((val (in-vector o)))
          (insert r val))))
    
    (define num 2)
    
    (let ((chp-lst (cons (make-thread) (build-list num make-place)))) ;; (list (cons <in-channel> <out-channel>) ...)
      (for ((v (in-list (directory-list "/proc"))) (chp (in-cycle (in-list chp-lst))))
        (send (cdr chp) v))
      (map (lambda (chp) (send (cdr chp) #f)) chp-lst)
      (output (apply append-result-vector (map (lambda (chp) (sync (car chp))) chp-lst))))))

(module* main racket/base
  (require (submod ".." helper))

  (main))
