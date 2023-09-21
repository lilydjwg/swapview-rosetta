#lang racket/base

(module shared racket/base
  (#%declare #:unsafe)
  (require (only-in racket/flonum fllog fl/ fl+ flexpt flfloor ->fl fl->exact-integer)
           (submod racket/performance-hint begin-encourage-inline)
           (only-in racket/file file->string)
           (only-in racket/format ~a ~r))
  (provide resolve-pid make-table table-update! print-table)

  (begin-encourage-inline
    ;; bytes and integers
    (define (digit? byte)
      (and (>= byte 48) (<= byte 57)))
    (define (get-integer bstr)
      (for/fold ((c 0))
                ((b (in-bytes bstr))
                 #:when (digit? b))
        (+ (* c 10) (- b 48))))
    (define (bytes-prefix? b p)
      (and (>= (bytes-length b) (bytes-length p))
           (for/and ((b1 (in-bytes b))
                     (b2 (in-bytes p)))
             (= b1 b2))))

    ;; strings
    (define (string-substitute! str o a)
      (for ((i (in-range 0 (string-length str)))
            #:when (char=? (string-ref str i) o))
        (string-set! str i a))
      str)

    ;; basic formatters
    (define (format-pid pid) (~a pid #:width 7 #:align 'right))
    (define (filesize size)
      (define n (* 1024 size))
      (if (< n 1100) (format "~aB" n)
          (letrec ([const-base (fllog 1024.0)]
                   [fn (->fl n)]
                   [p (flfloor (fl/ (fllog (fl/ fn 1100.0)) const-base))]
                   [s (~r (fl/ fn (flexpt 1024.0 (fl+ 1.0 p))) #:precision '(= 1))]
                   [unit (string-ref "KMGT" (fl->exact-integer p))])
            (format "~a~aiB" s unit))))
    (define (format-size size) (~a size #:width 9 #:align 'right))
    (define (resolve-cmdline s)
      (let ([l (string-length s)]
            [o #\u0]
            [c #\ ])
        (if (zero? l)
            s
            (string-substitute! (substring s 0 (- l 1)) o c))))
    (define (fmt1 s1 s2 s3)
      (string-append-immutable s1 " " s2 " " s3))
    (define (total n)
      (string-append "Total: " (~a n #:min-width 10 #:align 'right)))

    (define (get-smaps pid) (format "/proc/~a/smaps" pid))

    ;; readers
    (define (get-size input)
      (for/fold ((r 0))
                ((l (in-bytes-lines input 'any))
                 #:when (bytes-prefix? l #"Swap: "))
        (+ r (get-integer l))))
    (define (get-cmdline pid) (file->string (format "/proc/~a/cmdline" pid)))

    (define (resolve-pid pid)
      (with-handlers ([exn:fail:filesystem? (lambda (exn) #f)])
        (let ((v (call-with-input-file (get-smaps pid) get-size)))
          (if (zero? v)
              #f
              (cons v (fmt1 (format-pid pid) (format-size (filesize v)) (resolve-cmdline (get-cmdline pid))))))))

    ;; (hash/c exact-positive-integer? (listof string?))
    (define (make-table)
      (make-hasheq))
    (define (table-update! tbl k v)
      (define o (hash-ref tbl k #f))
      (cond (o (hash-set*! tbl k (cons v o)))
            (else (hash-set*! tbl k (list v)))))
    (define (print-table tbl)
      (define k-box (box 0))
      (displayln (fmt1 (format-pid "PID") (format-size "SWAP") "COMMAND"))
      (hash-for-each
       tbl
       (lambda (k v)
         (for-each displayln v)
         (set-box*! k-box (+ (unbox* k-box) (* (length v) k))))
       #t)
      (displayln (total (filesize (unbox* k-box)))))))

(module* helper racket/base
  (#%declare #:unsafe)
  (require (submod ".." shared)
           (only-in racket/place place/context place-channel-put place-channel))
  (provide main)

  (define (main)
    (define (make-place com-ch)
      (let ((pl (place/context ch
                  (for ((pid (in-producer sync #f ch)))
                    (define v (resolve-pid pid))
                    (cond (v (place-channel-put com-ch v))))
                  (place-channel-put com-ch #f))))
        pl))

    (define-values (out-channel in-channel) (place-channel))

    (define (send chp v)
      (place-channel-put chp v))
    (define (make-recv n)
      (let ((cnt-box (box n)))
        (lambda (chp)
          (let loop ()
            (sync
             (handle-evt
              chp
              (lambda (v)
                (cond (v)
                      ((= (unbox* cnt-box) 1) #f)
                      (else (set-box*! cnt-box (- (unbox* cnt-box) 1))
                            (loop))))))))))

    (define place-number 2)

    (let ((chp-lst (build-list place-number (lambda (_) (make-place out-channel)))))
      (parameterize ((current-directory "/proc"))
        (for ((v (in-directory #f (lambda (_) #f))) (chp (in-cycle (in-list chp-lst))))
          (send chp v)))
      (for-each (lambda (chp) (send chp #f)) chp-lst))

    (define tbl (make-table))

    (for ((v (in-producer (make-recv place-number) #f in-channel)))
      (table-update! tbl (car v) (cdr v)))
    (print-table tbl)))

(module* main racket/base
  (require (submod ".." helper))

  (main))
