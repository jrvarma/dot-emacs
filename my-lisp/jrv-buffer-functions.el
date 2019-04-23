;;; Various functions to Switch/Delete buffer and/or window
(provide 'jrv-buffer-functions)
(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/my-lisp/"))
(require 'jrv-settings)

(defun jrv/buffer-switch-buffer-other-window () 
  "Use ido-switch-buffer to change buffer in next window"
  (interactive) 
  (other-window 1)
  (ido-switch-buffer)      
  (other-window 1))

(defun jrv/buffer-kill-buffer-other-window () 
  "Kill buffer in next window (prompting if it is modified)"
  (interactive) 
  (other-window 1)
  (kill-buffer-ask (current-buffer))
  (other-window 1))

;; swap buffers between windows
;; http://stackoverflow.com/questions/1774832/how-to-swap-the-buffers-in-2-windows-emacs
(defun jrv/buffer-swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this (selected-window))
     (other (next-window))
     (this-buffer (window-buffer this))
     (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)
    (other-window 1)))

(defun jrv/buffer-dired-in-other-window (file) 
  "Open dired in next window prompting for directory"
  (interactive "fdired (directory): ") 
  (other-window 1)
  (dired file)
  (other-window 1))

(defun jrv/buffer-find-file-in-other-window (file) 
  "Open file in next window prompting for file"
  (interactive "FFind File: ") 
  (other-window 1)
  (find-file file)
  (other-window 1))

(defun jrv/buffer-kill-star-buffers ()
  "Kill buffers beginning with * unless they have a running process or are in jrv/settings-dont-kill-buffers"
  (interactive)
  (let ((count 0))
	(dolist(buffer (buffer-list))
      (when (string-match "^\\*" (buffer-name buffer))
        (unless (string-match jrv/settings-dont-kill-buffers (buffer-name buffer))
          (unless (get-buffer-process buffer)
            (when (kill-buffer buffer)
              (setq count (1+ count)))))))
    (message "%d buffer(s) killed" count)))

;; I have one frame named Emacs Notmuch for notmuch related buffers
;; and another frame named Emacs Std for other buffers
;; When switching buffers using ido:
;;    Show buffers matching jrv/settings-buffers-on-all-frames in both frames
;;    Do not show buffers beginning with * unless
;;    they have a running process or matcg jrv/settings-dont-ignore-star-buffers
;;    Show buffers related to motmuch mail only in Emacs Notmuch frame
;;    Show buffers other than above only in Emacs Std frame
(defun jrv/buffer-ido-ignore-buffers-framewise (name)
  (let* ((star-buffer (string-match-p "^*" name))
         (show-on-all-frames
          (string-match-p jrv/settings-buffers-on-all-frames name))
         (ignorable-star-buffer
          (and star-buffer
               (not show-on-all-frames)
               (not (string-match-p jrv/settings-dont-ignore-star-buffers name))
               (not (get-buffer-process name))))
           (notmuch-buffer
            (string-match-p "^\\*\\(notmuch\\|sent\\|unsent\\)" name))
           (this-frame-name
            (alist-get 'name (frame-parameters (selected-frame)))))
      (cond
       ((string= this-frame-name "Emacs Std")
        (or ignorable-star-buffer notmuch-buffer))
       ((string= this-frame-name "Emacs Notmuch")
        (not (or show-on-all-frames notmuch-buffer)))
       (t nil))))

