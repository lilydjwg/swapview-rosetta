#!/usr/bin/sbcl --script

(defun number-string-p (str)
  (= (length str) (second (multiple-value-list (parse-integer str :junk-allowed t)))))

(defun path-check (path)
  "Check the directory and files in the directory"
  (and (number-string-p (subseq path 6 (position #\/ path :from-end t)))
       (probe-file (concatenate 'string path "smaps"))
       (probe-file (concatenate 'string path "cmdline"))))

(defun get-swap-size (path)
  "Calculate the swap in smaps of the path"
 (let ((size (ignore-errors
            (with-open-file (stream path :if-does-not-exist nil)
              (if stream
                (loop for line = (read-line stream nil nil)
                      while line
		     when (search "Swap:" line)
                      summing (parse-integer (subseq line (1+ (position #\: line))) :junk-allowed t)))))))
		    (if size size 0)))

(defun convert-size (size)
  "For human beings"
  (cond
    ((> size (* 1024 1024)) (format nil "~1$GiB" (float (/ size 1024 1024))))
    ((> size 1024) (format nil "~1$MiB" (float (/ size 1024))))
    (t (format nil "~1$KiB" size))))

(defun print-result (list)
  "Print the result in list, which item: (id swap cmdline)"
  (let ((format-string "~7@A~7T~8@A~8T~A~%"))
    (progn
      (format t format-string "PID" "SWAP" "COMMAND")
      (loop for item in list
            summing (second item) into total
            do (format t format-string (first item) (convert-size (second item)) (third item))
	 finally (format t "Total: ~A~%" (convert-size total))))))

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
  "The main process function which read with path string, return multi-value"
  (let ((id (get-process-id path))
        (size (get-swap-size (concatenate 'string path "smaps")))
        (cmdline (get-cmdline (concatenate 'string path "cmdline"))))
    (values id size cmdline)))

(defun main ()
  (loop for x in (directory "/proc/*/")
      ; do (print x)
        if (and (path-check (namestring x))
		  (> (get-swap-size (concatenate 'string (namestring x) "smaps")) 0))
        collecting (multiple-value-list (process (namestring x))) into result-list
        finally (if (not (null result-list))
                  (print-result (sort result-list  #'< :key #'second)))))

(main)
