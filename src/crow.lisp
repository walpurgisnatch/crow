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

(defun substp (regex item)
    (declare (type simple-string regex item))
    (if (scan-to-strings regex item)
        t
        nil))

(defun ignore-item (item)
    (declare (type simple-string item))
    (or (null item)
        (numberp (parse-integer item :junk-allowed t))
        (string= "" item)
        (substp "http" item)        
        (member item *ignore-list* :test #'equal)))

(defun collect-from-file-old (from to)
    (with-open-file (input from)
        (with-open-file (output to :direction :output :if-exists :supersede)
            (loop for line = (read-line input nil)
                  while line do 
                    (let* ((parts (split-url (string-trim '(#\^M) line)))
                           (wordlist (create-wordlist parts)))
                        (loop for item in wordlist when (not (ignore-item item)) do
                          (progn (write-line item output)
                                 (pushnew item *ignore-list*))))))))


(defun collect-from-file (from to &optional (buffer-size 8192))
    (declare (optimize (speed 3) (safety 2))
             (type fixnum buffer-size))
    (let ((buffer (make-array buffer-size :element-type 'character))
          (end buffer-size)
          (temp buffer-size))
        (declare (type fixnum end temp))
        (with-open-file (in from)
            (with-open-file (output to :direction :output :if-exists :supersede)
                (loop
                  (when (< end buffer-size)
                      (return))
                  (setf (subseq buffer 0) (subseq buffer temp buffer-size))
                  (setf end (read-sequence buffer in :start (- buffer-size temp)))
                  (setf temp 0)
                  (dotimes (i end)
                      (declare (type fixnum i)
                               (dynamic-extent i))
                      (when (char-equal #\Newline
                                        (aref buffer i))
                          (let ((wordlist (create-wordlist (split-url (subseq buffer temp i)))))
                              (loop for item in wordlist when (not (or (null item) (ignore-item item))) do
                                (progn (write-line item output)
                                       (pushnew item *ignore-list*))))
                          (setf temp (1+ i)))))))))


(defun split-url (url)
    (declare (type simple-string url))
    (loop for item in (split "[.]|[/]|[?]" url)
          when (not (ignore-item item))
            collect item))

(defun create-wordlist (parts)
    (when parts
        (let ((args (get-args (car (last parts)))))
            (if args 
                (append (butlast parts)
                        (arguments args)
                        (arguments-values args))
                parts))))

(defun arguments (args)
    (mapcar #'car args))

(defun arguments-values (args)
     (mapcar #'cadr args))

(defun get-args (url)
    (declare (type simple-string url))
    (let ((parts (all-matches-as-strings "([a-zA-Z_%0-9-]*?)=.*?(&|$)" url)))
        (mapcar #'(lambda (part) (split "=" (string-trim "&" part))) parts)))
