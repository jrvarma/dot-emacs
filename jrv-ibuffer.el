(provide 'jrv-ibuffer)
(require 'ibuffer) 
(eval-when-compile (require 'ibuffer)) 

;;replace default with ibuffer. Open i other window, and take me there.
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)

;;sort on major-mode
(defvar ibuffer-default-sorting-mode)
(setq ibuffer-default-sorting-mode 'major-mode)

(defvar ibuffer-saved-filter-groups)
(setq ibuffer-saved-filter-groups
  (quote (("default"      
            ("Org" ;; all org-related buffers
              (mode . org-mode))  
            ("Mail"
              (or  ;; mail-related buffers
               (mode . message-mode)
               (mode . mail-mode)
               (mode . notmuch-search-mode)
               (mode . notmuch-hello-mode)
               (mode . notmuch-show-mode)
               ))
            ("Dired"
                (mode . dired-mode))
            ("Programming"
              (or
                (mode . ess-mode)
                (mode . LaTex-mode)
                (mode . python-mode)
                (mode . emacs-lisp-mode)
                ;; etc
                )) 
            ))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))

;;Don't show (filter) groups that are empty.
(defvar ibuffer-show-empty-filter-groups)
(setq ibuffer-show-empty-filter-groups nil)