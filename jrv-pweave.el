(provide 'jrv-pweave)

;; Plw-mode for Pweave Latex documents

(defvar ess-noweb-default-code-mode)
(defvar ess-noweb-doc-mode)
(defvar ess-noweb-mode-prefix-map)

(declare-function ess-swv-weave "ess-swv")
(declare-function ess-noweb-mode "ess-noweb-mode")

(add-to-list 'auto-mode-alist '("\\.Plw\\'" . Plw-mode))


(define-derived-mode Plw-mode ess-noweb-mode "Pweave"
  "Python code chunks in ESS-Noweb" 
  (setq ess-noweb-default-code-mode 'python-mode)
  (setq ess-noweb-doc-mode 'latex-mode)
  (define-key ess-noweb-mode-prefix-map "s" 'my-weave)
  (define-key ess-noweb-mode-prefix-map "x" 'pweave-inline)
)

(defun pweave-inline()
  "Insert the code for inline python expressions <%=%> and move cursor "
  (interactive)
  (insert "<%=%>")
  (backward-char 2))

(defun my-weave (&optional choose)
  "If the code-mode is python, run pweave. Else run ess-swv-weave for Sweave/knit

Optional parameter CHOOSE is passed on to pweave or ess-swv-weave.
"
  (interactive "P")
  (cond ((equal ess-noweb-default-code-mode 'python-mode) (pweave choose))
        (t (ess-swv-weave choose))))

(defun pweave (&optional choose)
  "Pweave file in current buffer

If CHOOSE is non-nil prompt for Pweave options, else use '-f tex'"
  (interactive "P")
  (let ((pweave-options (if choose
                          (read-from-minibuffer "Pweave options" nil nil nil nil "-f tex")
                        "-f tex")))
  (save-excursion
    (basic-save-buffer); do not Pweave old version of file !
    (let* ((plw-file (buffer-file-name))
           (plw-dir (file-name-directory plw-file))
           (py-buf (get-buffer-create "*Pweave Python Output*"))
           (pwv-status))
     (with-current-buffer py-buf (erase-buffer))
      (message "Running Pweave %s %s" pweave-options plw-file)
      (setq pwv-status
            (call-process-shell-command (concat "Pweave " pweave-options " " plw-file) nil py-buf t))
      (if (not (= 0 pwv-status))
          (message "** OOPS: error in Pweave (%d)!" pwv-status))
      (display-buffer py-buf)))))
