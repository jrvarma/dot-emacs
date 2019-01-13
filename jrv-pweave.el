(provide 'jrv-pweave)

;; Plw-mode for pweave Latex documents

(defvar ess-noweb-default-code-mode)
(defvar ess-noweb-doc-mode)
(defvar ess-noweb-mode-prefix-map)
(defvar ess-noweb-minor-mode-map)
(make-variable-buffer-local 'ess-noweb-minor-mode-map)

(declare-function ess-swv-weave "ess-swv")
(declare-function ess-noweb-mode "ess-noweb-mode")

(add-to-list 'auto-mode-alist '("\\.Plw\\'" . Plw-mode))
(add-to-list 'auto-mode-alist '("\\.pmd\\'" . pmd-mode))

(defun pmd-key-bindings()
  (interactive)
  (define-key ess-noweb-minor-mode-map "\M-ns" 'pweave-markdown)
  (define-key ess-noweb-minor-mode-map "\M-np" 'markdown-preview)
  (define-key ess-noweb-minor-mode-map "\M-nP" 'markdown-to-pdf)
  (define-key ess-noweb-minor-mode-map "\M-nx" 'python-inline)
  (define-key ess-noweb-minor-mode-map "\M-ny" 'pweave-list-inline-expressions))

(defun Plw-key-bindings()
  (interactive)
  (define-key ess-noweb-minor-mode-map "\M-ns" 'pweave-tex)
  (define-key ess-noweb-minor-mode-map "\M-nP" 'ess-swv-PDF)
  (define-key ess-noweb-minor-mode-map "\M-nx" 'python-inline)
  (define-key ess-noweb-minor-mode-map "\M-ny" 'pweave-list-inline-expressions))

(define-derived-mode Plw-mode ess-noweb-mode "pweave"
  "Python code chunks in ESS-Noweb" 
  (setq ess-noweb-default-code-mode 'python-mode)
  (setq ess-noweb-doc-mode 'latex-mode)
  (Plw-key-bindings))
  ;; ;; for some reason setting minor mode without delay does not work
  ;; (run-at-time 1 nil '(lambda() (Plw-key-bindings))))

(define-derived-mode pmd-mode ess-noweb-mode "pweavemarkdown"
  "Python code chunks in markdown using ESS-Noweb" 
  (setq ess-noweb-default-code-mode 'python-mode)
  (setq ess-noweb-doc-mode 'markdown-mode)
  (pmd-key-bindings))
  ;; ;; for some reason setting minor mode without delay does not work
  ;; (run-at-time 1 nil '(lambda() (pmd-key-bindings))))

(defun python-inline()
  "Insert the code for inline python expressions <%=%> and move cursor "
  (interactive)
  (insert "<%=%>")
  (backward-char 2))

(defun pweave-markdown (&optional choose)
  "pweave markdown file in current buffer

If CHOOSE is non-nil prompt for pweave options, else use '-f markdown'"
  (interactive "P")
  (pweave "markdown" choose))

(defun pweave-tex (&optional choose)
  "pweave latex file in current buffer

If CHOOSE is non-nil prompt for pweave options, else use '-f markdown'"
  (interactive "P")
  (pweave "tex" choose))

(defun pweave (doctype &optional choose)
  "pweave file in current buffer

If CHOOSE is non-nil prompt for pweave options, else use '-f doctype'"
  (interactive "P")
  (let* ((pwv-opt (concat "-f " doctype " -i noweb"))
         (pweave-options (if choose
                             (read-from-minibuffer "pweave options" pwv-opt nil nil nil pwv-opt)
                           pwv-opt)))
  (save-excursion
    (basic-save-buffer); do not pweave old version of file !
    (let* ((plw-file (buffer-file-name))
           (plw-dir (file-name-directory plw-file))
           (py-buf (get-buffer-create "*pweave Python Output*"))
           (pwv-status))
     (with-current-buffer py-buf (erase-buffer))
     (message "Running pweave %s %s" pweave-options plw-file)
     (setq pwv-status
           (call-process-shell-command (concat "pweave " pweave-options " " plw-file) nil py-buf t))
     (cond
      ((= 0 pwv-status) (message "done")) 
      (t (message "** OOPS: error in pweave (%d)!" pwv-status)
         (display-buffer py-buf)))))))

(defun pweave-list-inline-expressions ()
  "List all inline expressions in buffer or region" 
  (interactive)
  (let ((my-command
         "sed -e 's/<%/\\n<%/g' -e 's/%>/%>\\n/g' | grep '^<%' | sed -e 's/<%=//g' -e 's/%>//g' "))
    (if (use-region-p)
        (shell-command-on-region (region-beginning) (region-end) my-command nil)
      (shell-command-on-region (point-min) (point-max) my-command nil)))
    )


;; (defvar pmd-minor-mode-map
;;       (let ((map (make-sparse-keymap)))
;;         (define-key map (kbd "M-n s") 'pweave-markdown)
;;         (define-key map (kbd "M-n x") 'python-inline)
;;         (define-key map (kbd "M-n P") 'pweave-markdown)
;;         map)
;;       "Minor mode key map that modifies ess-no-web key bindings for pmd mode")

;; (defvar Plw-minor-mode-map
;;       (let ((map (make-sparse-keymap)))
;;         (define-key map (kbd "M-n s") 'pweave-tex)
;;         (define-key map (kbd "M-n x") 'python-inline)
;;         (define-key map (kbd "M-n P") 'ess-swv-PDF)
;;         map)
;;       "Minor mode key map that modifies ess-no-web key bindings for Plw mode")

;; (define-minor-mode Plw-minor-mode
;;   "modify ess-no-web key bindings for Plw mode")

;; (define-minor-mode pmd-minor-mode
;;   "modify ess-no-web key bindings for pmd mode")

