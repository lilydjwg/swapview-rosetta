;;; optimization version for speed, sbcl specific
(declaim (optimize (speed 3) (compilation-speed 0) (debug 0) (safety 0) (space 0)))

(declaim (inline number-string-p))
(defun number-string-p (str)
  (declare (type (simple-array character) str))
  (= (length str) (the fixnum (second (multiple-value-list (parse-integer str :junk-allowed t))))))

(declaim (inline path-check))
(defun path-check (path)
  "Check the directory and files in the directory"
  (declare (type (simple-array character) path))
  (and (number-string-p (subseq path 6 (position #\/ path :from-end t)))
       (probe-file (concatenate 'string path "smaps"))
       (probe-file (concatenate 'string path "cmdline"))))

(declaim (ftype (function ((simple-array character)) fixnum) get-swap-size))
(declaim (inline get-swap-size))
(defun get-swap-size (path)
  "Calculate the swap in smaps of the path"
  (or (ignore-errors
        (with-open-file (stream path :if-does-not-exist nil)
          (if stream
            (loop for line = (read-line stream nil nil)
                  while (and line  (search "Swap" (the (simple-array character) line)))
                  summing (the fixnum (parse-integer line :start (the fixnum (1+ (position #\: line))) :junk-allowed t)))
            0)))
      0))

(declaim (ftype (function (fixnum) (simple-array character)) convert-size))
(declaim (inline convert-size))
(defun convert-size (size)
  "For human beings"
  (cond
    ((> size 1048576) (format nil "~1$GiB" (/ size 1048576)))
    ((> size 1024) (format nil "~1$MiB" (/ size 1024)))
    (t (format nil "~1$KiB" size))))

(declaim (inline print-result))
(defun print-result (lis)
  "Print the result in list, which item: (id swap cmdline)"
  (let ((format-string "~5@A~7T~8@A~8T~A~%"))
   (declare (type (simple-array character) format-string))
    (format t format-string "PID" "SWAP" "COMMAND")
    (loop for item in lis
          summing (second item) into (the fixnum total)
          do (format t format-string (first item) (convert-size (second item)) (third item))
          finally (format t "Total: ~A~%" (convert-size total)))))

(declaim (ftype (function ((simple-array character)) (simple-array character)) get-process-id))
(declaim (inline get-process-id))
(defun get-process-id (path)
  "Return process id from the path"
  (subseq path 6 (position #\/ path :from-end t)))

(declaim (ftype (function ((simple-array character)) (simple-array character)) get-cmdline))
(defun get-cmdline (path)
  "Return process cmdline"
  (declare (type (simple-array character) path))
  (with-open-file (stream path :if-does-not-exist nil)
    (if stream
      (substitute #\Space #\Nul (read-line stream nil nil))
      "")))

(declaim (inline process))
(defun process (path)
  "The main process function which read with path string, return: id, size, cmdline"
  (declare (type (simple-array character) path))
  (values (get-process-id path)
          (get-swap-size (concatenate 'string path "smaps"))
          (get-cmdline (concatenate 'string path "cmdline"))))

(defun main ()
  (loop for x in (directory "/proc/*/")
        ; do (print x)
        if (and (path-check (native-namestring x))
                (> (the fixnum (get-swap-size (concatenate 'string (native-namestring x) "smaps"))) 0))
        collecting (multiple-value-list (process (native-namestring x))) into result-list
        finally (if result-list
                  (print-result (sort result-list  #'< :key #'second)))))
;(main)
