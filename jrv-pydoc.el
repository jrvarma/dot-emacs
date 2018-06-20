;; modified from
;; http://emacs.stackexchange.com/questions/19422/library-for-automatically-inserting-python-docstring-in-google-style
;; produces numpy style docstring

(provide 'jrv-pydoc)
(declare-function string-join "subr-x")
(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string
     "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))
(defun get-function-definition(sentence)
  "Get current function definition"
  (if (string-match "def.*?(.*?):" sentence)
      (match-string 0 sentence)))
(defun get-parameters(sentence)
  "Get parameters in current function definition"
  (let ((y (get-function-definition sentence)))
    (if y
        (if (string-match "(.*)" y)
            (match-string 0 y)))))
(autoload 'thing-at-point "thingatpt" nil t) ;; built-in library
(defun python-insert-docstring()
  "Insert docstring for current function in numpy style"
  (interactive)
  (let* ((s0 (thing-at-point 'sentence))
         (s (string-join (split-string s0 "\n") " "))
         (p (get-parameters s))
         (params (split-string p "[?\,?\(?\)?\ ]"))
         (S (thing-at-point 'line))
         (tab (progn
                (string-match "^\\(.*def \\)" S)
                (make-string (length (match-string 1 S)) ? )))
         (first-param t)
         param)
  (when params
    (forward-line 1)
    (insert tab) (insert "r\"\"\"\n")
    (while params
      (let ((param (chomp (car params))))
        (when (and (/= (length param) 0) (not (string-match "^self$" param)))
          (when first-param
            (insert "\n") (insert tab) (insert "Parameters\n")
            (insert tab) (insert "----------\n")
            (setq first-param nil))
          (insert tab) (insert param) (insert " : \n")))
      (setq params (cdr params)))
    (insert "\n") (insert tab) (insert "Returns\n")
    (insert tab) (insert "-------\n")
    (insert "\n") (insert tab) (insert "Examples\n")
    (insert tab) (insert "--------\n\n")
    (insert tab) (insert "\"\"\"\n"))))
