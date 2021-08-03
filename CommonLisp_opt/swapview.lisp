;#!/usr/bin/sbcl --script
;(declaim (optimize speed))

(defun number-string-p (str)
  ; (declare (type (simple-array character) str))
  (= (length str) (second (multiple-value-list (parse-integer str :junk-allowed t)))))

(defun path-check (path)
  "Check the directory and files in the directory"
  (and (number-string-p (subseq path 6 (position #\/ path :from-end t)))
       (probe-file (concatenate 'string path "smaps"))
       (probe-file (concatenate 'string path "cmdline"))))

(defun get-swap-size (path)
  "Calculate the swap in smaps of the path"
  (or (ignore-errors
        (with-open-file (stream path :if-does-not-exist nil)
          (if stream
            (loop for line = (read-line stream nil nil)
                  while line
                  when (search "Swap:" line)
                  summing (parse-integer (subseq line (1+ (position #\: line))) :junk-allowed t))
            0)))
      0))

(defun convert-size (size)
  "For human beings"
  (cond
    ((> size 1048576) (format nil "~1$GiB" (/ size 1048576)))
    ((> size 1024) (format nil "~1$MiB" (/ size 1024)))
    (t (format nil "~1$KiB" size))))

(defun print-result (lis)
  "Print the result in list, which item: (id swap cmdline)"
  (let ((format-string "~7@A~7T~9@A~9T~A~%"))
    (format t format-string "PID" "SWAP" "COMMAND")
    (loop for item in lis
          summing (second item) into total
          do (format t format-string (first item) (convert-size (second item)) (third item))
          finally (format t "Total: ~10@A~%" (convert-size total)))))

(defun get-process-id (path)
  "Return process id from the path"
  (subseq path 6 (position #\/ path :from-end t)))

(defun get-cmdline (path)
  "Return process cmdline"
  (with-open-file (stream path :if-does-not-exist nil)
    (if stream
      (substitute #\Space #\Nul (read-line stream nil nil))
      "")))

(defun process (path)
  "The main process function which read with path string, return: id, size, cmdline"
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
