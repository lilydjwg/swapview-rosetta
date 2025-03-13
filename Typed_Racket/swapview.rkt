#lang typed/racket/base

(module shared typed/racket
  (require typed/racket/flonum)
  (provide resolve-pid make-table table-update! print-table
           Record Table PID)

  ;; types
  (define-type PID (U Exact-Positive-Integer Path-String))
  (define-type Record (Pairof Exact-Positive-Integer String))
  (define-type Table (HashTable Exact-Nonnegative-Integer (Listof String)))

  ;; bytes and integers
  (: get-integer (-> Bytes Exact-Nonnegative-Integer))
  (define (get-integer bstr)
    (for/fold ((c : Exact-Nonnegative-Integer 0))
              ((b (in-bytes bstr)))
      (define d (- b 48))
      (if (and (>= d 0) (<= d 9)) (+ (* c 10) d) c)))
  (: bytes-prefix? (-> Bytes Bytes Boolean))
  (define (bytes-prefix? b p)
    (and (>= (bytes-length b) (bytes-length p))
         (for/and ((b1 (in-bytes b))
                   (b2 (in-bytes p)))
           (= b1 b2))))
  ;; strings
  (: string-substitute! (-> String Char Char String))
  (define (string-substitute! str o a)
    (for ((i (in-range 0 (string-length str)))
          #:when (char=? (string-ref str i) o))
      (string-set! str i a))
    str)

  ;; basic formatters
  (: format-pid (-> PID String))
  (define (format-pid pid) (~a pid #:width 7 #:align 'right))
  (: filesize (-> Exact-Nonnegative-Integer String))
  (define (filesize size)
    (define n (* 1024 size))
    (if (< n 1100) (format "~aB" n)
        (letrec ([const-base (fllog 1024.0)]
                 [fn (->fl n)]
                 [p (flfloor (fl/ (fllog (fl/ fn 1100.0)) const-base))]
                 [s (~r (fl/ fn (flexpt 1024.0 (fl+ 1.0 p))) #:precision '(= 1))]
                 [unit (string-ref "KMGT" (fl->exact-integer p))])
          (format "~a~aiB" s unit))))
  (: format-size (-> String String))
  (define (format-size size) (~a size #:width 9 #:align 'right))
  (: resolve-cmdline (-> String String))
  (define (resolve-cmdline s)
    (let ([l (string-length s)]
          [o #\u0]
          [c #\ ])
      (if (zero? l)
          s
          (string-substitute! (substring s 0 (- l 1)) o c))))
  (: fmt1 (-> String String String String))
  (define (fmt1 s1 s2 s3)
    (string-append-immutable s1 " " s2 " " s3))
  (: total (-> String String))
  (define (total n)
    (string-append "Total: " (~a n #:min-width 10 #:align 'right)))

  (: get-smaps (-> PID String))
  (define (get-smaps pid) (format "/proc/~a/smaps" pid))

  ;; readers
  (: get-size (-> Input-Port Exact-Nonnegative-Integer))
  (define (get-size input)
      (for/fold ((r : Exact-Nonnegative-Integer 0))
                ((l (in-bytes-lines input 'any))
                 #:when (bytes-prefix? l #"Swap: "))
        (+ r (get-integer l))))
  (define (get-cmdline pid) (file->string (format "/proc/~a/cmdline" pid)))

  (: resolve-pid (-> PID (U False Record)))
  (define (resolve-pid pid)
    (with-handlers ([exn:fail:filesystem? (lambda (exn) #f)])
      (let ((v (call-with-input-file (get-smaps pid) get-size)))
        (if (zero? v)
            #f
            (cons v (fmt1 (format-pid pid) (format-size (filesize v)) (resolve-cmdline (get-cmdline pid))))))))

  (: make-table (-> Table))
  (define (make-table)
    (make-hasheq))
  (: table-update! (-> Table Exact-Nonnegative-Integer String AnyValues))
  (define (table-update! tbl k v)
    (define o (hash-ref tbl k #f))
    (cond (o (hash-set*! tbl k (cons v o)))
          (else (hash-set*! tbl k (list v)))))
  (: print-table (-> Table AnyValues))
  (define (print-table tbl)
    (define k-box ((inst box Exact-Nonnegative-Integer) 0))
    (displayln (fmt1 (format-pid "PID") (format-size "SWAP") "COMMAND"))
    (hash-for-each
     tbl
     (lambda ((k : Exact-Nonnegative-Integer) (v : (Listof String)))
       ((inst for-each String) displayln v)
       (set-box! k-box (+ (unbox k-box) (* (length v) k))))
     #t)
    (displayln (total (filesize (unbox k-box))))))

(module* main typed/racket
  (require (submod ".." shared))
  (require/typed racket/base
    (in-directory (-> (U Path-String False) (-> Path-String Boolean) (Sequenceof Path-String))))

  (: main (-> AnyValues))
  (define (main)
    (define tbl (make-table))
    (parameterize ((current-directory "/proc"))
      (for ((pid : Path-String (in-directory #f (lambda ((_ : Path-String)) #f))))
        (define pair (resolve-pid pid))
        (cond (pair (table-update! tbl (car pair) (cdr pair))))))
    (print-table tbl))

  (main)
  )
