;; my auctex customizations
(provide 'jrv-auctex-config)

;; (defvar TeX-command-list)

(defvar TeX-auto-save)
(setq TeX-auto-save t)
(defvar TeX-parse-self)
(setq TeX-parse-self t)
(defvar TeX-PDF-mode)
(setq-default TeX-PDF-mode t)
(defvar TeX-engine)
(setq-default TeX-engine 'xetex)
(defvar TeX-force-default-mode)
(setq TeX-force-default-mode t)
(defvar LaTeX-command-style)
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout) ")))

(defun texcount ()
  (interactive)
  (let*
    ( (this-file (buffer-file-name))
      (word-count
        (with-output-to-string
          (with-current-buffer standard-output
            (call-process "texcount" nil t nil "-1" this-file)
    ) ) ) )
    (message word-count)
) )

;; this function uses a python script beamer-ho.py to add watermarks etc
;; beamer-ho.py is assumed to be in the path
(defun make-handout-pdf()
  "Compile handout PDF from underlying tex file"
  (interactive)
  (when (buffer-modified-p)
    (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " 
                          (buffer-name)))
        (save-buffer)))  
  (message "Making handout file in background.")
  (set-process-sentinel 
   (start-process "beamer-ho" "*beamer-ho*" "beamer-ho.py" 
                  (file-name-sans-extension (buffer-file-name)))
   '(lambda (process event) (message "Making handout file status: %s" event))))

(defun latex-keydef(which-map)
  (define-key which-map [(control ?c) (control ?h)] 'TeX-help)
  (define-key which-map [(control ?c) (?w)] 'texcount)
  (define-key which-map [(control ?c) (?h)] 'make-handout-pdf))

(defvar LaTeX-mode-hook)
(defvar LaTeX-mode-map)
(add-hook 'LaTeX-mode-hook '(lambda () (latex-keydef LaTeX-mode-map)))

;; see outline-magic.el
(defvar outline-promotion-headings)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq outline-promotion-headings
                  '("\\chapter" "\\section" "\\subsection"
                    "\\subsubsection" "\\paragraph" "\\subparagraph"))))

(defvar TeX-view-program-selection)                  
(setq TeX-view-program-selection
      (quote
       (((output-dvistyle-pstricks)  "xdg-open") (output-dvi "xdg-open") 
        (output-pdf "xdg-open") (output-html "xdg-open"))))

(defvar my-latex-help-file) ;; from my-settings
(defun TeX-help ()
  "Display the Not so short guide to Latex"
  (interactive)
  (message "Opening latex not too short guide in browser")
  (start-process "latex-help" "*latex-help*" "xdg-open" my-latex-help-file))

