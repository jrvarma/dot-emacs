;;; dired mode customizations and its key bindings 
(provide 'jrv-dired-config)
                          
(require 'dired-sort-map)               ; s s, s x, s t or s n to sort by Size, eXtension, Time or Name
(require 'my-settings)
(defvar my-minimal)
(require 'jrv-dired-compare nil my-minimal)    ; compare two directories by time
(require 'jrv-shell-commands nil my-minimal)   ; execute programs, launch files in associated programs, send file to jrv-hdrive

(setq dired-dwim-target t)              ; default to other dired window for copy and compare              

(defun my-dired-switches (unset)
  "Set or (with prefix argument unset) dired listing switches"
  (interactive "P")
  (if unset
      (setq dired-listing-switches "-al")
    (setq dired-listing-switches "-ahl --time-style=+%e%b%y%t%H:%M --group-directories-first")))

(my-dired-switches nil)

(defvar dired-omit-files) 
(setq-default dired-omit-files-p t) ; this is buffer-local variable
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))


(put 'dired-find-alternate-file 'disabled nil)   ; enable 'a' to visit folder in same buffer
                                        
(add-hook 'dired-mode-hook              ; some key bindings in dired
      (lambda ()
 (define-key dired-mode-map [?,] 'dired-search-file-by-start-letter)
 (define-key dired-mode-map [?l] 'dired-launch-file) ; Launch file at cursor in associated program (l for launch)
 (define-key dired-mode-map [?\;] 'dired-launch-file) ; Launch file at cursor in associated program (l for launch)
 (define-key dired-mode-map [(meta control !)] 'dired-execute-file-in-shell)  ; Execute file at cursor (in a terminal)
 (define-key dired-mode-map [(meta control &)] 'dired-execute-file)  ; Execute file at cursor
 (define-key dired-mode-map [(control f9)] 'dired-compare-directories-new) ; Compare directory with other window
 (define-key dired-mode-map [(control =)] 'dired-diff) ; = is assigned to ediff
 (define-key dired-mode-map [?W] 'dired-copy-filename-with-backslash-as-kill) ; Insert filename with backslash/slash
 (define-key dired-mode-map [?2] 'dired-dos2unix) ; Run dos2unix on selected files
 (define-key dired-mode-map [?c] 'dired-new-file) ; Create new file in new directory
 (define-key dired-mode-map [?`] 'dired-revert-buffer-clean-slate) ; revert dired buffer without subdirs, marks etc
))

;; functions for dired key bindings (see also jrv-shell-commands.el)
(defun dired-copy-filename-with-backslash-as-kill ()
(interactive)
  "Copy to kill-ring the full path of filename at cursor"
(let ((fnpath (dired-get-filename)))
  (kill-new fnpath)
  (message "%s" fnpath)))

(defun dired-search-file-by-start-letter(ch)
  "Reads character and searches for first file starting with that letter"
(interactive "cFile starting with: ")
(cond                                   ;
 ((if (search-forward-regexp (concat "[0-9][0-9]:[0-9][0-9] +" (char-to-string ch)) nil t) 
                      (progn (backward-char) t)))
 ((if (search-backward-regexp (concat "[0-9][0-9]:[0-9][0-9] +" (char-to-string ch)) nil t) 
                      (progn (search-forward (char-to-string ch)) (backward-char) t)))
 (t (message (concat "No file beginning with " (char-to-string ch)))))) 

;; add guesses for command to be run on file based on extension
(defvar dired-guess-shell-alist-user)
(setq dired-guess-shell-alist-user
      '(("\\.py\\'" "python")
        ("\\.pdf\\'" "evince" "xournal" "okular")))
(defun dired-revert-buffer-clean-slate ()
  (interactive)
  (let ((this-dir dired-directory))
    (when this-dir
      (kill-buffer nil)
      (dired this-dir))))
