(in-package :cl-user)
(defpackage crow
  (:use :cl)
  (:import-from :cl-ppcre                
   				:scan-to-strings   
                :all-matches-as-strings                
   				:split)
  (:export :collect-from-file))

(in-package :crow)

(defparameter *ignore-list* '("" "http" "and" "or" "on" "the" "a" "com" "ru" "net"))

(defun substp (regex item)
    (if (scan-to-strings regex item)
        t
        nil))

(defun ignore-item (item)
    (or (null item)
        (and (stringp item)
             (or (string= "" item)
                 (substp "http" item)))
        (numberp (parse-integer item :junk-allowed t))
        (member item *ignore-list* :test #'equal)))

(defun collect-from-file (from to)
    (with-open-file (input from)
        (with-open-file (output to :direction :output :if-exists :supersede)
            (loop for line = (read-line input nil)
                  while line do 
                    (let* ((parts (split-url (string-trim '(#\^M) line)))
                           (wordlist (create-wordlist parts)))
                        (loop for item in wordlist when (not (ignore-item item)) do
                          (progn (write-line item output)
                                 (pushnew item *ignore-list*))))))))

(defun split-url (url)
    (let ((items (split "[.]|[/]|[?]" url)))
        (loop for item in items
              collect item)))

(defun create-wordlist (parts)
    (append (butlast parts)
            (arguments-from-url parts)
            (arguments-values-from-url parts)))

(defun arguments-from-url (list)
    (mapcar #'car (get-args (car (last list)))))

(defun arguments-values-from-url (list)
     (mapcar #'car (mapcar #'cdr (get-args (car (last list))))))

(defun get-args (url)
    (let ((parts (all-matches-as-strings "([a-zA-Z_%0-9-]*?)=.*?(&|$)" url)))
        (mapcar #'(lambda (part) (split "=" (string-trim "&" part))) parts)))
