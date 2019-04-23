(provide 'jrv-pweave)
(eval-and-compile  ;; suppress compiler warnings
  (require 'package)
  (setq package-enable-at-startup nil)
  (package-initialize)
  ;; (add-to-list 'load-path "~/.emacs.d/my-lisp/")
  (require 'ess-swv)
  (require 'ess-noweb)
)

(define-derived-mode Plw-mode ess-noweb-mode "pweave"
  "Python code chunks in ESS-Noweb" 
  (setq ess-noweb-default-code-mode 'python-mode)
  (setq ess-noweb-doc-mode 'latex-mode)
  (make-local-variable 'ess-noweb-minor-mode-map)
  (define-key ess-noweb-minor-mode-map "\M-ns"
    'jrv/pweave-weave-tex)
  (define-key ess-noweb-minor-mode-map "\M-nP"
    'ess-swv-PDF)
  (define-key ess-noweb-minor-mode-map "\M-nx"
    'jrv/pweave-python-inline)
  (define-key ess-noweb-minor-mode-map "\M-ny"
    'jrv/pweave-list-inline-expressions))

(define-derived-mode pmd-mode ess-noweb-mode "pweavemarkdown"
  "Python code chunks in markdown using ESS-Noweb" 
  (setq ess-noweb-default-code-mode 'python-mode)
  (setq ess-noweb-doc-mode 'markdown-mode)
  (make-local-variable 'ess-noweb-minor-mode-map)
  (define-key ess-noweb-minor-mode-map "\M-ns"
    'jrv/pweave-weave-markdown)
  (define-key ess-noweb-minor-mode-map "\M-np"
    'markdown-preview)
  (define-key ess-noweb-minor-mode-map "\M-nP"
    'jrv/markdown-to-pdf)
  (define-key ess-noweb-minor-mode-map "\M-nx"
    'jrv/pweave-python-inline)
  (define-key ess-noweb-minor-mode-map "\M-ny"
    'jrv/pweave-pweave-list-inline-expressions))

(defun jrv/pweave-python-inline()
  "Insert the code for inline python expressions <%=%> and move cursor "
  (interactive)
  (insert "<%=%>")
  (backward-char 2))

(defun jrv/pweave-weave-markdown (&optional choose)
  "pweave markdown file in current buffer

   If CHOOSE is non-nil prompt for pweave options, 
   else use '-f markdown'"
  (interactive "P")
  (jrv/pweave-weave "markdown" choose))

(defun jrv/pweave-weave-tex (&optional choose)
  "pweave latex file in current buffer

   If CHOOSE is non-nil prompt for pweave options, 
   else use '-f markdown'"
  (interactive "P")
  (jrv/pweave-weave "tex" choose))

(defun jrv/pweave-weave (doctype &optional choose)
  "pweave file in current buffer

   If CHOOSE is non-nil prompt for pweave options, 
   else use '-f doctype'"
  (interactive "P")
  (let* ((pwv-opt (concat "-f " doctype " -i noweb"))
         (pweave-options
          (if choose (read-from-minibuffer
                      "pweave options" pwv-opt nil nil nil pwv-opt)
            pwv-opt)))
  (save-excursion
    (basic-save-buffer); do not pweave old version of file !
    (let* ((plw-file (buffer-file-name))
           (plw-dir (file-name-directory plw-file))
           (py-buf (get-buffer-create "*pweave Python Output*"))
           (pwv-status))
     (with-current-buffer py-buf (erase-buffer))
     (message "Running pweave %s %s" pweave-options plw-file)
     (setq pwv-status (call-process-shell-command
                       (concat "pweave " pweave-options " " plw-file)
                       nil py-buf t))
     (cond
      ((= 0 pwv-status) (message "done")) 
      (t (message "** OOPS: error in pweave (%d)!" pwv-status)
         (display-buffer py-buf)))))))

(defun jrv/pweave-list-inline-expressions ()
  "List all inline expressions in buffer or region" 
  (interactive)
  (let ((my-command
         "sed -e 's/<%/\\n<%/g' -e 's/%>/%>\\n/g' | grep '^<%' | sed -e 's/<%=//g' -e 's/%>//g' "))
    (if (use-region-p)
        (shell-command-on-region
         (region-beginning) (region-end) my-command nil)
      (shell-command-on-region
       (point-min) (point-max) my-command nil)))
    )
