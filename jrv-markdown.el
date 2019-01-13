;;; markdown mode
 
(provide 'jrv-markdown)
(require 'my-settings)
(defvar my-minimal) ; from my-settings

(defvar markdown-mode-map)
(declare-function markdown-export-file-name  "markdown-mode")
(declare-function markdown  "markdown-mode")

;; use pandoc to process markdown
(defvar markdown-command)
(setq markdown-command "pandoc --ascii -f markdown -t html")
;; in .pmd files, run Pweave first before running pandoc
(make-variable-buffer-local 'markdown-command)
(add-hook 'markdown-mode-hook
          '(lambda ()
             (when (string-match "\.[pP]md$" (buffer-name))
               (setq markdown-command "pwv-pandoc"))))

(defun markdown-export-blog ()
  "Export markdown to .blog file"
  (interactive)
  (let ((buf (current-buffer))
        (outbuf (generate-new-buffer "*blogfile*"))
        (outname (markdown-export-file-name ".blog")))
    (set-buffer buf)
    (markdown (buffer-name outbuf))
    (set-buffer outbuf)
    (goto-char (point-min))
    (re-search-forward "<p>\\(.*?\\)</p>") ; title is inside a <p></p> tag
    (replace-match "\\1")                  ; remove these tags
    (write-file outname)
    (kill-buffer outbuf)
    (switch-to-buffer buf)))

(declare-function find-file-in-other-window "jrv-buffer-functions")
(defun markdown-to-pdf (prompt)
  "Export markdown to pdf file and display it in pdf viewer"
  (interactive "P")
  (let ((buf (current-buffer))
        (outbuf (generate-new-buffer "*pdffile*"))
        (outname (markdown-export-file-name ".pdf"))
        (old-markdown-command markdown-command)
        (markdown-failed nil)
        (pandoc "pandoc")
        (pandoc-opt "-V 'mainfont:Nimbus Roman' --pdf-engine=xelatex -f markdown"))
    (when prompt
        (setq pandoc-opt
              (read-string "Pandoc options: " pandoc-opt nil pandoc-opt)))
    (when (string-match "\.[pP]md$" (buffer-name))
      (setq pandoc "pwv-pandoc"))
    (set-buffer buf)
    (setq markdown-command (concat pandoc " " pandoc-opt " -o " outname))
    (message "Running pandoc")
    (condition-case err
        (markdown (buffer-name outbuf))
         ;; The handler.
         (error
          (message "%s" (error-message-string err))
          (setq markdown-failed t)
          ;; markdown-command is buffer local
          ;; so we must change it before switching buffer
          (setq markdown-command old-markdown-command)
          (switch-to-buffer outbuf)))
    (unless markdown-failed
      (setq markdown-command old-markdown-command)
      (message "Pandoc finished")
      (if (> (buffer-size outbuf) 0)
          (switch-to-buffer outbuf)
        (kill-buffer outbuf)
        (find-file-in-other-window outname)))))
        ;; (let ((process-connection-type nil)) 
        ;;   (start-process "*launch*" nil "xdg-open" outname))
        ;; (switch-to-buffer buf)))))


(defun markdown-key-bindings()
  (define-key markdown-mode-map [(meta f3)] 'markdown-preview)
  (define-key markdown-mode-map [(control c) (control c) ?b] 
    'markdown-export-blog)
  (define-key markdown-mode-map [(control c) (control c) ?P] 
    'markdown-to-pdf)
  (when (require 'jrv-html nil my-minimal)
    (define-key markdown-mode-map [(control \")] 'html-double-quote)
    (define-key markdown-mode-map [(control \')] 'html-single-quote)
    (define-key markdown-mode-map [(control -)] 'html-ndash))
)
(add-hook 'markdown-mode-hook  'markdown-key-bindings)

