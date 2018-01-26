;;; markdown mode
 
(provide 'jrv-markdown)
(require 'my-settings)
(defvar my-minimal) ; from my-settings

(defvar markdown-mode-map)
(declare-function markdown-export-file-name  "markdown-mode")
(declare-function markdown  "markdown-mode")
(defvar markdown-command)
(setq markdown-command "pandoc --ascii -f markdown -t html")

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

(defun markdown-to-pdf (prompt)
  "Export markdown to pdf file and display it in pdf viewer"
  (interactive "P")
  (let ((buf (current-buffer))
        (outbuf (generate-new-buffer "*pdffile*"))
        (outname (markdown-export-file-name ".pdf"))
        (old-markdown-command markdown-command)
        (pandoc "pandoc -V 'mainfont:Nimbus Roman' --pdf-engine=xelatex -f markdown"))
    (setq pandoc
          (if prompt
              (read-string "Pandoc command: " pandoc nil pandoc)
            pandoc))
    (cond
     ((string-match "\.pmd$" (buffer-name))
      (let ((mdfile (replace-regexp-in-string "\.pmd" ".md" (buffer-name))))
        (message "Running pandoc")
        (shell-command (concat pandoc " -o " outname " " mdfile) outbuf)))
     (t         
      (set-buffer buf)
      (setq markdown-command (concat pandoc " -o " outname))
      (message "Running pandoc")
      (markdown (buffer-name outbuf))
      (message "Pandoc finished")
      (setq markdown-command old-markdown-command)))
    (if (> (buffer-size outbuf) 0)
        (switch-to-buffer outbuf)
      (kill-buffer outbuf)
      (let ((process-connection-type nil)) 
        (start-process "*launch*" nil "xdg-open" outname))
      (switch-to-buffer buf))))


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
