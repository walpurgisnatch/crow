(in-package :cl-user)
(defpackage crow
  (:use :cl)
  (:import-from :cl-ppcre                
   				:scan-to-strings   
                :all-matches-as-strings                
                :split)
  (:export :collect-from-file))

(in-package :crow)

(defparameter *ignore-list* '("" "http" "and" "or" "on" "the" "a" "com" "ru" "net" "org"))
(defparameter *dirs* (make-hash-table :test 'equalp))
(defparameter *args* (make-hash-table :test 'equalp))

(defun substp (regex item)
    (declare (type simple-string regex item))
    (if (scan-to-strings regex item)
        t
        nil))

(defun ignore-item (item)
    (declare (type simple-string item))
    (or (numberp (parse-integer item :junk-allowed t))
        (string= "" item)
        (substp "http" item)))

(defun collect-from-file (from dirs args &optional (buffer-size 8192))
    (declare (optimize (speed 3) (safety 2))
             (type fixnum buffer-size))
    (let ((buffer (make-array buffer-size :element-type 'character))
          (end buffer-size)
          (temp buffer-size))
        (declare (type fixnum end temp))
        (with-open-file (in from)
            (loop
              (when (< end buffer-size)
                  (write-to dirs *dirs*)
                  (write-to args *args*)
                  (return))
              (setf (subseq buffer 0) (subseq buffer temp buffer-size))
              (setf end (read-sequence buffer in :start (- buffer-size temp)))
              (setf temp 0)
              (dotimes (i end)
                  (declare (type fixnum i)
                           (dynamic-extent i))
                  (when (char-equal #\Newline
                                    (aref buffer i))
                      (let ((uri (subseq buffer temp i)))
                          (create-wordlist uri (split-url uri)))
                      (setf temp (1+ i))))))))

(defun write-to (file table)
    (with-open-file (output file :direction :output :if-exists :supersede)
        (maphash #'(lambda (key value) (declare (ignorable key))
                       (write-line value output)) table)))

(defun split-url (url)
    (declare (type simple-string url))
    (loop for item in (split "[.]|[/]|[?]" url)
          when (not (ignore-item item))
            collect item))

(defun create-wordlist (url parts)
    (when parts
        (if (parse-args url)
            (sethash *dirs* (butlast parts))
            (sethash *dirs* parts))))
                    
(defun parse-args (url)
    (declare (type simple-string url))
    (handler-case 
        (let ((args (quri:uri-query-params (quri:uri url))))
            (when args
                (mapcar #'(lambda (item) (sethash *args* (list (car item) (cdr item)))) args)))
      (error nil)))

(defun sethash (table entries)
    (loop for entry in entries
          when (and (not (null entry))
                    (not (ignore-item entry))
                    (null (nth-value 1 (gethash entry table))))
            do (setf (gethash entry table) entry)))
