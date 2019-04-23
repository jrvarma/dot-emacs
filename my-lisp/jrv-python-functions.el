(provide 'jrv-python-functions)

(eval-and-compile  ;; suppress compiler warnings
  (require 'package)
  (setq package-enable-at-startup nil)
  (package-initialize)
  (add-to-list 'load-path "~/.emacs.d/my-lisp/")
  (require 'jrv-settings)
  (require 'flycheck)
)

(defun jrv/python-flycheck()
  "Start flycheck mode in python buffers unless Plw or pmd"
  (interactive)
  (if buffer-file-name
      (unless (string-match
               (file-name-extension buffer-file-name) "(Plw)|(pmd)$")
        (flycheck-mode))))

(defun jrv/python-help ()
  "Launch offline python help in browser or windows help file"
  (interactive)
  (message "Opening python help in browser")
  (start-process "python-help" "*python-help*"
                 "xdg-open" jrv/settings-offline-python-help-file))


;; produce numpy style docstring template
;; modified from
;; http://emacs.stackexchange.com/questions/19422/library-for-automatically-inserting-python-docstring-in-google-style
(declare-function string-join "subr-x") ; suppress compiler warning
(defun jrv/python-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string
     "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))
(defun jrv/python-get-function-definition(sentence)
  "Get current function definition"
  (if (string-match "def.*?(.*?):" sentence)
      (match-string 0 sentence)))
(defun jrv/python-get-parameters(sentence)
  "Get parameters in current function definition"
  (let ((y (jrv/python-get-function-definition sentence)))
    (if y
        (if (string-match "(.*)" y)
            (match-string 0 y)))))
(autoload 'thing-at-point "thingatpt" nil t) ;; built-in library
(defun jrv/python-insert-docstring()
  "Insert docstring for current function in numpy style"
  (interactive)
  (let* ((s0 (thing-at-point 'sentence))
         (s (string-join (split-string s0 "\n") " "))
         (p (jrv/python-get-parameters s))
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
      (let ((param (jrv/python-chomp (car params))))
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
