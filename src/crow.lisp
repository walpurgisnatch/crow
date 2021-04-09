(in-package :cl-user)
(defpackage crow
  (:use :cl)
  (:import-from :cl-ppcre                
   				:scan-to-strings   
                :all-matches-as-strings                
   				:split)
  (:export :collect-from-file))

(in-package :crow)


(defparameter *common* '("and" "or" "on" "the" "a" "com" "ru" "net"))


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
        (member item *common*)))


(defmacro read-from-write-to (file to &body body)
    `(with-open-file (input ,file)
         (with-open-file (output ,to :direction :output :if-exists :supersede)
             (loop for line = (read-line input nil)
                   while line do (progn ,@body)))))

(defun collect-from-file (from to)
        (read-from-write-to from to
            (let* ((parts (split-url (string-trim '(#\^M) line)))
                   (wordlist (create-wordlist parts)))
                (loop for item in wordlist when (not (ignore-item item)) do
                  (write-line item output)))))

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
