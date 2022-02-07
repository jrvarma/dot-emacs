(provide 'jrv-python-functions)

(eval-and-compile  ;; suppress compiler warnings
  (require 'package)
  (setq package-enable-at-startup nil)
  (package-initialize)
  (add-to-list 'load-path "~/.emacs.d/my-lisp/")
  (require 'jrv-settings)
  (require 'flycheck)
)

(defun jrv/python/flycheck()
  "Start flycheck mode in python buffers unless Plw or pmd"
  (interactive)
  (if buffer-file-name
      (unless (string-match
               (file-name-extension buffer-file-name) "(Plw)|(pmd)$")
        (flycheck-mode))))

(defun jrv/python/help (beg end)
  "Launch offline python stack help in browser"
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let* ((qry (if (and beg end)
                (buffer-substring-no-properties beg end)
              ""))
         (choice (read-key-sequence
                  "Python | Numpy | panDas | Matplotlib | Scipy"))
         (urls '(("p" . "pydoc") ("n" . "npdoc") ("d" . "pddoc")
                 ("m" . "mpldoc") ("s" . "scipydoc")))
         (url (concat "file:///0/tp/search/"
                      (assoc-default (downcase choice) urls)
                      "?q="
                      qry)))
    (start-process "python-help" "*python-help*"
                   "/usr/bin/firefox" url)))


;; produce numpy style docstring template
;; inspired by 
;; http://emacs.stackexchange.com/questions/19422/library-for-automatically-inserting-python-docstring-in-google-style

(defun jrv/python/chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string
     "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(declare-function show-paren--default "paren")
(defun jrv/python/fun-parameters()
  "Find function parameters and associated stuff"
  (interactive)
  (let (parens left-paren colon parameters def tab)
    (save-excursion
      (setq parameters nil)
      (setq tab "")
      (when (re-search-forward "):" nil t 1)
        (setq colon (match-end 0))
        (setf (point) (- colon 1))
        (setq parens (show-paren--default))
        (when parens
          (setq left-paren (nth 3 parens))
          (setq parameters
                (buffer-substring-no-properties
                 (- (point) 1) left-paren))
          (setf (point) left-paren)
          (beginning-of-line)
          (setq def
                (buffer-substring-no-properties (point) left-paren))
          (string-match "^\\(.*def \\)" def)
          (setq tab
                (replace-regexp-in-string "def" "   "
                                          (match-string 1 def))))))
    (list colon parameters tab)))

(defun jrv/python/insert-docstring()
  "Insert docstring for current function in numpy style"
  (interactive)
  (let* ((fp (jrv/python/fun-parameters))
         (params (split-string (nth 1 fp) "[?\,?\(?\)?\ ]"))
         (tab (nth 2 fp))
         (first-param t)
         param)
    (when params
      (setf (point) (nth 0 fp))
      (forward-line 1)
      (insert tab) (insert "r\"\"\"\n")
      (while params
        (let ((param
               (replace-regexp-in-string
                "=.*" "" (jrv/python/chomp (car params)))))
          (when (and (/= (length param) 0)
                     (not (string-match "^self$" param)))
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
