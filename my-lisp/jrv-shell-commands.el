;; various shell commands run from dired buffer
(provide 'jrv-shell-commands)

(require 'dired-aux) ;; suppress compiler warning

(defun jrv/shell-command-in-shell (file)
  "Execute file in terminal instead of directly as in shell-command
This allows the executable file to prompt for and respond to user input"
  (interactive "fExecute file in terminal: ")
  (if (not (file-executable-p "terminator"))
      (error "Command terminator not available"))
  (when (y-or-n-p (format "%s %s" "Execute " file))
    (async-shell-command (format "%s %s" "terminator -x bash -c"  file))))

(defun jrv/shell-send-buffer-file-to-h ()
  "Send file in the buffer to cloud. Prompts if buffer needs saving."
  (interactive)
  (if (not (executable-find "2h"))
      (error "Command 2h not available"))
  (if (buffer-modified-p) 
      (message "Please save file first. Buffer has been modified")
    (message (concat "2h "
                     (file-name-nondirectory (buffer-file-name))
                     "  Please wait ..."))
    (message (shell-command-to-string
              (concat "2h " (file-name-nondirectory (buffer-file-name)))))))


(defun jrv/shell-command-on-buffer-file(command &optional arg file-list)
  "Run shell command on the file in buffer. Prompts if buffer needs saving."
  (interactive
   (let ((files (list (file-name-nondirectory (buffer-file-name)))))
     (list
      (dired-read-shell-command "& on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (if (buffer-modified-p) 
      (message "Please save file first. Buffer has been modified")
    (async-shell-command (concat command " " (buffer-file-name) " &"))))

