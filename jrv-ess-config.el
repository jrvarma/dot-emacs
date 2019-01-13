(provide 'jrv-ess-config)

(declare-function ess-rutils-html-docs "ess-rutils.el")
(defvar inferior-R-args)
(setq inferior-R-args "--no-restore-history --no-save")
(defvar ess-ask-for-ess-directory)
(setq ess-ask-for-ess-directory nil)
(defvar ess-swv-processor)
(setq ess-swv-processor 'knitr)
(require 'my-settings)
(defvar my-minimal)
(when (require 'jrv-mypaths nil my-minimal)
  (defvar ess-history-directory)
  (defvar jrv-jtemp)
  (when jrv-jtemp) 
    (setq ess-history-directory (concat jrv-jtemp "/.R/")))
(defvar comint-scroll-to-bottom-on-input)
(setq comint-scroll-to-bottom-on-input t) ; scroll R window to bottom after each evaluation
(defvar comint-scroll-show-maximum-output)
(setq comint-scroll-show-maximum-output t)
(defvar comint-scroll-to-bottom-on-output)
(setq comint-scroll-to-bottom-on-output t)
(defvar ess-smart-S-assign-key)
(setq ess-smart-S-assign-key nil)

(add-hook 'ess-mode-hook
     '(lambda () 
        (define-key ess-mode-map [(control ?c) (control ?h)] 'my-r-help)
))

;; In Rnw and Pwv files, define key  "C-c H" to make-handout-pdf (jrv-auctex.el)
(add-hook 'ess-noweb-mode-hook
     '(lambda () 
        (define-key ess-noweb-mode-prefix-map [(?H)] 'make-handout-pdf)))
 
;; The code to determine whether there is an active region is based on
;; http://stackoverflow.com/questions/10594208/how-do-i-get-region-selection-programmably-in-emacs-lisp
(declare-function ess-execute "ess-inf.el")
(defun my-r-help(beg end)
  "Start html help. Help on topic if region highlighted"
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (and beg end)
      (ess-execute (concat "help('" (buffer-substring-no-properties beg end) "', help_type='html')") 
                   'buffer)
    (ess-execute "help.start()")))
    ;; (ess-rutils-html-docs))) This seems to have a bug

(defvar ess-swv-pdflatex-commands)
(setq ess-swv-pdflatex-commands '("xelatex" "pdflatex" "texi2pdf" "make"))
(defvar ess-pdf-viewer-pref)
(setq ess-pdf-viewer-pref "emacsclient")
;; setq does not work for TeX-source-correlate-mode so we customize
;; (custom-set-variables '(TeX-source-correlate-mode t))
(defun ess-bibtex ()
   "Run bibtex on tex files underlying the Rnw file in the buffer."
   (interactive)
   (shell-command (concat "bibtex " (replace-regexp-in-string "\.Rnw" "" (buffer-name)))))

(add-to-list 'auto-mode-alist '("\\.Rmd$" . poly-markdown+r-mode))

