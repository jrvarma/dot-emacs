(provide 'jrv-ess-functions)

;;;;;;;;; suppress compiler warnings ;;;;;;;;;;;
(declare-function ess-rutils-html-docs "ess-rutils.el")
(declare-function ess-execute "ess-inf.el")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jrv/ess-r-help(beg end)
  "Start html help. Help on topic if region highlighted"
  ;; The code to determine whether there is an active region is based on
  ;; http://stackoverflow.com/questions/10594208/how-do-i-get-region-selection-programmably-in-emacs-lisp
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (and beg end)
      (ess-execute (format "help('%s', help_type='html')"
                           (buffer-substring-no-properties beg end)) 
                   'buffer)
    (ess-execute "help.start()")))


(defun jrv/ess-bibtex ()
   "Run bibtex on tex files underlying the Rnw file in the buffer."
   (interactive)
   (shell-command (concat "bibtex " (replace-regexp-in-string "\.Rnw" "" (buffer-name)))))
