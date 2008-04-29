(declaim (optimize (safety 0) (debug 0) (speed 3) (space 0)))
  
(defpackage :scanner
  (:use :common-lisp :series))
  
(in-package :scanner)
  
(series::install)
  
(defun parse-text (filename)
  (print
   (hash-table-count
    (multiple-value-bind (keys values)
	(series:map-fn
	 '(values string string)
	 #'(lambda (line)
	     (declare (type (simple-array character (*)) line))
	     (let ((first-tilde (position #\~ line))
		   (last-tilde (1+ (position #\~ line :from-end t))))
	       (values (subseq line (1+ first-tilde) last-tilde)
		       (subseq line (1+ last-tilde)))))
	 (series:scan-file filename #'read-line))
      (series:collect-hash keys values :test #'equal)))))
