;; various shell commands run from dired buffer
(provide 'jrv-shell-commands)
(require 'dired)

(defun dired-dos2unix ()
  "Run dos2unix -k on current file in dired buffer"
  (interactive)
  (dired-do-async-shell-command  "dos2unix -k " nil (dired-get-marked-files)))

(defun dired-new-file (fname)
  "Create new empty file"
  (interactive "sFile to Create: ")
  (message (shell-command-to-string 
           (concat "touch '" (dired-current-directory) fname "'")))
  (revert-buffer t t t))

(defun dired-launch-file ()
  "Launch system associated program on current file in dired buffer"
  ;; modified from
  ;; http://omniorthogonal.blogspot.in/2008/05/useful-emacs-dired-launch-hack.html
  (interactive)
  (let ((process-connection-type nil)) 
    (start-process "*launch*" nil "xdg-open" (dired-get-filename))))

(defun dired-execute-file ()
  "The current file in dired buffer is executed"
  (interactive)
  (when (y-or-n-p (format "%s %s" "Execute " (dired-get-filename)))
    (async-shell-command (dired-get-filename) )))

(defun dired-execute-file-in-shell ()
  "Execute in terminal: The current file in dired buffer is executed"
  (interactive)
  (when (y-or-n-p (format "%s %s" "Execute in terminal " (dired-get-filename)))
    (start-process "*exec-in-shell*"  nil
                   "terminator" "-x" "bash" "-c" (dired-get-filename))))

(defun shell-command-in-shell (file)
  "Execute file in terminal instead of directly as in shell-command
This allows the executable file to prompt for and respond to user input"
  (interactive "fExecute file in terminal: ")
  (when (y-or-n-p (format "%s %s" "Execute " file))
    (async-shell-command (format "%s %s" "terminator -x bash -c"  file))))

;; probably useless for anybody else
(defun send-buffer-file-to-h ()
  "Send file in the buffer to hdrive. Prompts if buffer needs saving."
  (interactive)
  (if (buffer-modified-p) 
      (message "Please save file first. Buffer has been modified")
    (message (concat "2h " (file-name-nondirectory (buffer-file-name)) "  Please wait ..."))
    (message (shell-command-to-string (concat "2h " (file-name-nondirectory (buffer-file-name)))))))

