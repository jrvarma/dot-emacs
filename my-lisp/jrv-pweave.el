(provide 'jrv-pweave)
(eval-and-compile  ;; suppress compiler warnings
  (require 'package)
  (setq package-enable-at-startup nil)
  (package-initialize)
  ;; (add-to-list 'load-path "~/.emacs.d/my-lisp/")
  ;; (require 'ess-swv)
  ;; (require 'ess-noweb)
  (require 'poly-noweb)
  (require 'poly-markdown)
  (require 'polymode)
)

(defcustom poly-noweb-innermode-python
  (clone poly-noweb-innermode
         :name "noweb-python"
         :mode 'python-mode)
  "Noweb for Python"
  :group 'poly-innermodes
  :type 'object)

(define-polymode poly-pmd-mode poly-markdown-mode
  "Python markdown mode (Pweave)"
  :lighter " PM-pmd"
  :innermodes '(poly-noweb-innermode-python)
  :keymap '(("\M-nx" . jrv/pweave/python-inline)
             ("\M-ny" . jrv/pweave/list-inline-expressions)))

(define-polymode poly-Plw-mode poly-latex-root-polymode
  "Python LaTex mode (Pweave)"
  :lighter " PM-Plw"
  :hostmode 'poly-noweb-latex-hostmode
  :innermodes '(poly-noweb-innermode-python :inherit)
  :keylist '(("\M-nx" . jrv/pweave/python-inline)
             ("\M-ny" . jrv/pweave/list-inline-expressions)))

;; (makunbound 'poly-pweave-weaver)
(defcustom poly-pweave-weaver
  (pm-shell-weaver :name "Pweave"
                   :from-to
                   '(("plw2tex" "\\.Plw\\'" "tex" "Pweave to LaTeX"
                      "pweave -f tex -i noweb %i -o %o")
                     ("pmd2md" "\\.pmd\\'" "md"
                      "Pweave to markdown"
                      "pweave -f markdown -i noweb %i -o %o")))
  "Pweave weaver."
  :group 'polymode-weave
  :type 'object)

(polymode-register-weaver poly-pweave-weaver t
                          poly-pmd-polymode poly-Plw-polymode)

;; (makunbound 'poly-jrv-pandoc-exporter-from-md)
(defcustom poly-jrv-pandoc-exporter-from-md
  (pm-shell-exporter :name "jrv-Pandoc-md"
                     :from
                     '(("markdown"  "\\.md\\'" "Markdown"
                        "pandoc --ascii -f markdown -F pantable -o %o %t %i"))
                     :to
                     '(("html" "html" "html document" "")
                       ("pdf" "pdf" "pdf document" "")
                       ("word" "docx" "word document" "")
                       ;; ("beamer" "pdf" "beamer slides"
                       ;;  "-t beamer --template /home/jrvarma/.pandoc/templates/default.beamer --pdf-engine xelatex")
                       ("beamer" "pdf" "beamer slides"
                        ("md2beamer %i"))
                       ("handout" "handout.pdf" "beamer handout"
                        ("md2beamer %i ho"))
                       ("unstamped" "unstamped.pdf" "beamer unstamped"
                        ("md2beamer %i no"))))
  "Pandoc exporter"
  :group 'polymode-export
  :type 'object)

;; (makunbound 'poly-jrv-pandoc-exporter-from-tex)
(defcustom poly-jrv-pandoc-exporter-from-tex
  (pm-shell-exporter :name "jrv-Pandoc-tex"
                     :from
                     '(("latex"  "\\.tex\\'" "LaTex"
                        "pandoc --ascii -f latex %t -o %o %t %i"))
                     :to
                     '(("html" "html" "html document" "")
                       ("pdf" "pdf" "pdf document" "")
                       ("word" "docx" "word document" "")
                       ("beamer" "pdf" "beamer slides"
                        "-t beamer --template /home/jrvarma/.pandoc/templates/default.beamer --pdf-engine xelatex")))
  "Pandoc exporter"
  :group 'polymode-export
  :type 'object)

(defcustom poly-jrv-Plw-exporter
  (pm-shell-exporter :name "jrv-Plw-exporter"
                     :from
                     '(("latex"  "\\.tex\\'" "LaTex"
                        ""))
                     :to
                     '(
                       ("pdf" "pdf" "pdf"
                        ("3pass-tex.py %i"))
                       ("handout" "handout.pdf" "beamer handout"
                        ("beamer-ho.py %i"))
                       ("unstamped" "unstamped.pdf"
                        "beamer unstamped handout"
                        ("beamer-ho.py %t %i -n"))))
  "Plw-exporter"
  :group 'polymode-export
  :type 'object)

(polymode-register-exporter poly-jrv-pandoc-exporter-from-md t
                            poly-pmd-polymode)
(polymode-register-exporter poly-jrv-pandoc-exporter-from-tex nil)
(polymode-register-exporter poly-jrv-handout-from-tex nil)
(polymode-register-exporter poly-jrv-Plw-exporter t poly-Plw-polymode)

(defun jrv/pweave/python-inline()
  "Insert the code for inline python expressions <%=%> and move cursor "
  (interactive)
  (insert "<%=%>")
  (backward-char 2))

(defun jrv/pweave/list-inline-expressions ()
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

;; (add-to-list 'auto-mode-alist '("\\.pmd" . poly-pweave-mode))
;; (setq auto-mode-alist (delete '("\\.pmd\\'" . pmd-mode) auto-mode-alist))
;; (define-derived-mode Plw-mode ess-noweb-mode "pweave"
;;   "Python code chunks in ESS-Noweb" 
;;   (setq ess-noweb-default-code-mode 'python-mode)
;;   (setq ess-noweb-doc-mode 'latex-mode)
;;   (make-local-variable 'ess-noweb-minor-mode-map)
;;   (define-key ess-noweb-minor-mode-map "\M-ns"
;;     'jrv/pweave/weave-tex)
;;   (define-key ess-noweb-minor-mode-map "\M-nP"
;;     'ess-swv-PDF)
;;   (define-key ess-noweb-minor-mode-map "\M-nx"
;;     'jrv/pweave/python-inline)
;;   (define-key ess-noweb-minor-mode-map "\M-ny"
;;     'jrv/pweave/list-inline-expressions))

;; (define-derived-mode pmd-mode ess-noweb-mode "pweavemarkdown"
;;   "Python code chunks in markdown using ESS-Noweb" 
;;   (setq ess-noweb-default-code-mode 'python-mode)
;;   (setq ess-noweb-doc-mode 'markdown-mode)
;;   (make-local-variable 'ess-noweb-minor-mode-map)
;;   (define-key ess-noweb-minor-mode-map "\M-ns"
;;     'jrv/pweave/weave-markdown)
;;   (define-key ess-noweb-minor-mode-map "\M-np"
;;     'markdown-preview)
;;   (define-key ess-noweb-minor-mode-map "\M-nP"
;;     'jrv/markdown/to-pdf)
;;   (define-key ess-noweb-minor-mode-map "\M-nx"
;;     'jrv/pweave/python-inline)
;;   (define-key ess-noweb-minor-mode-map "\M-ny"
;;     'jrv/pweave/pweave-list-inline-expressions))

;; (defun jrv/pweave/weave-markdown (&optional choose)
;;   "pweave markdown file in current buffer

;;    If CHOOSE is non-nil prompt for pweave options, 
;;    else use '-f markdown'"
;;   (interactive "P")
;;   (jrv/pweave/weave "markdown" choose))

;; (defun jrv/pweave/weave-tex (&optional choose)
;;   "pweave latex file in current buffer

;;    If CHOOSE is non-nil prompt for pweave options, 
;;    else use '-f markdown'"
;;   (interactive "P")
;;   (jrv/pweave/weave "tex" choose))

;; (defun jrv/pweave/weave (doctype &optional choose)
;;   "pweave file in current buffer

;;    If CHOOSE is non-nil prompt for pweave options, 
;;    else use '-f doctype'"
;;   (interactive "P")
;;   (let* ((pwv-opt (concat "-f " doctype " -i noweb"))
;;          (pweave-options
;;           (if choose (read-from-minibuffer
;;                       "pweave options" pwv-opt nil nil nil pwv-opt)
;;             pwv-opt)))
;;   (save-excursion
;;     (basic-save-buffer); do not pweave old version of file !
;;     (let* ((plw-file (buffer-file-name))
;;            (plw-dir (file-name-directory plw-file))
;;            (py-buf (get-buffer-create "*pweave Python Output*"))
;;            (pwv-status))
;;      (with-current-buffer py-buf (erase-buffer))
;;      (message "Running pweave %s %s" pweave-options plw-file)
;;      (setq pwv-status (call-process-shell-command
;;                        (concat "pweave " pweave-options " " plw-file)
;;                        nil py-buf t))
;;      (cond
;;       ((= 0 pwv-status) (message "done")) 
;;       (t (message "** OOPS: error in pweave (%d)!" pwv-status)
;;          (display-buffer py-buf)))))))


