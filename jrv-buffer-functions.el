;;; Various functions to Switch/Delete buffer and/or window
(provide 'jrv-buffer-functions)

(require 'ido)
(ido-mode 'buffers)

(defun switch-buffer-other-window () 
  "Use ido-switch-buffer to change buffer in next window"
  (interactive) 
  (other-window 1)
  (ido-switch-buffer)      
  (other-window 1))

(defun kill-buffer-other-window () 
  "Kill buffer in next window (prompting if it is modified)"
  (interactive) 
  (other-window 1)
  (kill-buffer-ask (current-buffer))
  (other-window 1))

;; swap buffers between windows
;; http://stackoverflow.com/questions/1774832/how-to-swap-the-buffers-in-2-windows-emacs
(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this (selected-window))
     (other (next-window))
     (this-buffer (window-buffer this))
     (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)
    (other-window 1)))

(defun dired-in-other-window (file) 
  "Open dired in next window prompting for directory"
  (interactive "fdired (directory): ") 
  (other-window 1)
  (dired file)
  (other-window 1))

(defun find-file-in-other-window (file) 
  "Open file in next window prompting for file"
  (interactive "FFind File: ") 
  (other-window 1)
  (find-file file)
  (other-window 1))

(defvar my-dont-kill-buffers) ; from my-settings
(defun kill-star-buffers ()
  "Kill buffers beginning with * unless they have a running process or are in my-dont-kill-buffers"
  (interactive)
  (let ((count 0))
	(dolist(buffer (buffer-list))
      (when (string-match "^\\*" (buffer-name buffer))
        (unless (string-match my-dont-kill-buffers (buffer-name buffer))
          (unless (get-buffer-process buffer)
            (when (kill-buffer buffer)
              (setq count (1+ count)))))))
    (message "%d buffer(s) killed" count)))

(declare-function escreen-current-screen-number "escreen")
;(defvar escreen-current-screen-number)
(defvar my-buffers-on-all-screens) ; from my-settings
(defvar my-dont-ignore-star-buffers) ; from my-settings

;; Function below is used when escreen is running with two screens (0 and 1)
;; Screen 1 is used for notmuch mail and Screen 0 for all others
;; When switching buffers using ido:
;;    Buffers specified in my-buffers-on-all-screens are shown in both screens
;;    Buffers beginning with * are not shown in any screen unless
;;       they have a running process or are in my-dont-ignore-star-buffers
;;    Buffers related to motmuch mail are shown only in Screen 1
;;    Buffers other than above are shown only in Screen 0
(defun ido-ignore-buffers-screenwise (name)
  (if (not (featurep 'escreen)) nil         
    (let* ((star-buffer (string-match-p "^*" name))
           (show-on-all-screens (string-match-p my-buffers-on-all-screens name))
           (ignorable-star-buffer
            (and star-buffer
                 (not show-on-all-screens)
                 (not (string-match-p my-dont-ignore-star-buffers name))
                 (not (get-buffer-process name))))
           (notmuch-buffer
            (string-match-p "^\\*\\(notmuch\\|sent\\|unsent\\)" name)))
      (cond
       ((eq (escreen-current-screen-number) 0)
        (or ignorable-star-buffer notmuch-buffer))
       ((eq (escreen-current-screen-number) 1)
        (not (or show-on-all-screens notmuch-buffer)))
       (t nil)))))

(setq ido-ignore-buffers '("\\` " ido-ignore-buffers-screenwise))

;; functions no longer in use

;; (defun delete-other-windows-and-buffer () 
;; "Kill buffer in next window (prompting if modified) and also kill window"
;;   (interactive) 
;;   (other-window 1)
;;   (kill-current-buffer-and-window))

;; (defun kill-current-buffer-and-window ()
;;   "Kill the current buffer (prompting if it is modified) and its window."
;;   (interactive)
;;   (kill-buffer-ask (current-buffer))
;;   (delete-window))

