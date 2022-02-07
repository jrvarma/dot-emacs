(provide 'jrv-dired-functions)

(require 'dired)     ;; suppress compiler warning
(require 'dired-aux) ;; suppress compiler warning

(defun jrv/dired/copy-filename-with-backslash-as-kill ()
  "Copy to kill-ring the full path of filename at cursor"
  (interactive)
  (unless (string= major-mode "dired-mode")
    (error "This function is available only in dired-mode"))
  (let ((fnpath (dired-get-filename)))
    (kill-new fnpath)
    (message "%s" fnpath)))

(defun jrv/dired/search-file-by-start-letter(ch)
  "Reads char and searches for first file starting with that char"
  (interactive "cFile starting with: ")
  (unless (string= major-mode "dired-mode")
    (error "This function is available only in dired-mode"))
  (cond                                   ;
   ((search-forward-regexp
     (concat "[0-9][0-9]:?[0-9][0-9] +" (char-to-string ch)) nil t) 
    (progn (backward-char) t))
  ((search-backward-regexp
    (concat "[0-9][0-9]:?[0-9][0-9] +" (char-to-string ch)) nil t)
   (progn (search-forward (char-to-string ch)) (backward-char) t))
  (t (message "No file beginning with %s" (char-to-string ch)))))

(defun jrv/dired/revert-buffer-clean-slate ()
  "Reload dired buffer from clean slate"
  (interactive)
  (unless (string= major-mode "dired-mode")
    (error "This function is available only in dired-mode"))
  (let ((this-dir dired-directory))
    (when this-dir
      (kill-buffer nil)
      (dired this-dir))))

(defun jrv/dired/dos2unix ()
  "Run dos2unix -k on current file in dired buffer"
  (interactive)
  (unless (string= major-mode "dired-mode")
    (error "This function is available only in dired-mode"))
  (dired-do-async-shell-command  "dos2unix -k "
                                 nil (dired-get-marked-files)))

(defun jrv/dired/decrypt-pdf ()
  "Run decrypt-pdf.sh on current file in dired buffer"
  (interactive)
  (unless (string= major-mode "dired-mode")
    (error "This function is available only in dired-mode"))
  (dired-do-async-shell-command  "decrypt-pdf.sh * "
                                 nil (dired-get-marked-files)))

(defun jrv/dired/new-file (fname)
  "Create new empty file"
  (interactive "sFile to Create: ")
  (unless (string= major-mode "dired-mode")
    (error "This function is available only in dired-mode"))
  (message (shell-command-to-string 
           (concat "touch '" (dired-current-directory) fname "'")))
  (revert-buffer t t t))

(defun jrv/dired/launch-file ()
  "Launch system associated program on current file in dired buffer
modified from http://omniorthogonal.blogspot.in/2008/05/useful-emacs-dired-launch-hack.html"
  (interactive)
  (unless (string= major-mode "dired-mode")
    (error "This function is available only in dired-mode"))
  (let ((process-connection-type nil)) 
    (start-process "*launch*" nil "xdg-open" (dired-get-filename))))

(defun jrv/dired/execute-file ()
  "The current file in dired buffer is executed"
  (interactive)
  (unless (string= major-mode "dired-mode")
    (error "This function is available only in dired-mode"))
  (when (y-or-n-p (format "%s %s" "Execute " (dired-get-filename)))
    (async-shell-command (dired-get-filename) )))

(defun jrv/dired/execute-file-in-shell ()
  "Execute in terminal: The current file in dired buffer is executed"
  (interactive)
  (unless (string= major-mode "dired-mode")
    (error "This function is available only in dired-mode"))
  (if (not (file-executable-p "terminator"))
      (error "Command terminator not available"))
  (when (y-or-n-p (format "%s %s" "Execute in terminal "
                          (dired-get-filename)))
    (start-process "*exec-in-shell*"  nil
                   "terminator" "-x" "bash" "-c" (dired-get-filename))))

(defun jrv/dired/async-shell-command(command &optional arg file-list)
  "Run async-shell-command on selected files in dired "
  (interactive
   (let ((files (list (dired-get-filename))))
     (list
      (dired-read-shell-command "& on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (async-shell-command (concat command " " (dired-get-filename) " &")))

(defvar jrv/dired/dired-listing-switches)
(defvar jrv/dired/dired-listing-switches-classify)
;; earlier listing switches were as follows
;; (defvar jrv/init/dired-listing-switches
;;   "-ahl --time-style=+%e%b%y%t%H:%M --group-directories-first")
;; but this breaks directory-listing-before-filename-regexp
;; that emacs uses to parse the listing and
;; this regexp is too complex to fix easily
(setq jrv/dired/dired-listing-switches
      "-Ahl --group-directories-first")
(setq jrv/dired/dired-listing-switches-classify
        (concat jrv/dired/dired-listing-switches " --classify"))

(defun jrv/dired/toggle-dired-classify()
  "Toggle the --classify switch in dired-listing-switches"
  (interactive)
  (if (string-equal
       dired-listing-switches
       jrv/dired/dired-listing-switches)
      (setq dired-listing-switches
            jrv/dired/dired-listing-switches-classify)
    (setq dired-listing-switches
          jrv/dired/dired-listing-switches)))
