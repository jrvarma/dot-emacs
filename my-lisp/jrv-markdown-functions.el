(provide 'jrv-markdown-functions)

(eval-and-compile  ;; this is all to suppress compiler warning
  (require 'package)
  (setq package-enable-at-startup nil)
  (package-initialize)
  (require 'markdown-mode)
)

(defun jrv/markdown/export-blog ()
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

(defun jrv/markdown/to-pdf (prompt)
  "Export markdown to pdf file and display it in pdf viewer"
  (interactive "P")
  (let ((buf (current-buffer))
        (outbuf (generate-new-buffer "*pdffile*"))
        (outname (markdown-export-file-name ".pdf"))
        (old-markdown-command markdown-command)
        (markdown-failed nil)
        (pandoc "pandoc")
        (pandoc-opt (concat ;; "-V 'mainfont=Nimbus Roman' "
                            "--pdf-engine=xelatex "
                            "-F pantable "
                            "-f markdown ")))
    (when prompt
        (setq pandoc-opt
              (read-string "Pandoc options: " pandoc-opt nil pandoc-opt)))
    (when (string-match "\.[pP]md$" (buffer-name))
      (setq pandoc "pwv-pandoc"))
    (when (string-match "\.[rR]md$" (buffer-name))
      (setq pandoc "knitr-pandoc"))
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
        (other-window 1)
        (find-file outname)
        (other-window 1)))))

