(provide 'jrv-auctex-functions)

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/my-lisp/"))
(require 'jrv-settings)

(defun jrv/auctex-texcount ()
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
(defun jrv/auctex-make-handout-pdf()
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
   '(lambda (process event)
      (if (eq event "finished")
          (message "Making handout file succeeded")
        (switch-to-buffer "*beamer-ho*")))))

(defun jrv/auctex-TeX-help ()
  "Display the Not so short guide to Latex"
  (interactive)
  (message "Opening latex not too short guide in browser")
  (start-process "latex-help" "*latex-help*"
                 "xdg-open" jrv/settings-latex-help-file))
