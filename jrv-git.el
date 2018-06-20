;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THIS CODE IS HIGHLY SPECIFIC TO MY NEEDS                 ;;
;; AND IS PROBABLY USELESS TO ANYBODY ELSE                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'jrv-git)
;;;; daily backups (with git)

(defun n-days-old (filename ndays)
  (< (float-time (nth 5 (file-attributes filename))) 
     (- (float-time) (* ndays 24 3600))))

(defun local-git-copy (local-file remote-file ndays)
  (when (or (not (file-exists-p local-file)) (n-days-old local-file ndays))
    (message "local-git-copy %s ==> %s" remote-file local-file)
    (find-file remote-file)
    (write-region nil nil local-file)
    (kill-buffer (get-file-buffer remote-file))
    (message (shell-command-to-string 
              (format "bash -c 'cd %s; git commit --message=%s-%s %s'" 
                      (file-name-directory local-file) 
                      (format-time-string "%Y-%m-%d")
                      (file-name-nondirectory local-file) 
                      (file-name-nondirectory local-file))))))

(defun local-git-copy-filelist (local-folder remote-folder file-list ndays)
  (mapc (lambda (x)
          (let* ((y (replace-regexp-in-string ".gpg$" "" x))
                 (local-file (concat local-folder "/" y))
                 (remote-file (concat remote-folder "/" x)))
            (local-git-copy local-file remote-file ndays)))
        file-list))

  ;; http://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/
(declare-function magit-mode-get-buffers "magit-mode")
(declare-function magit-restore-window-configuration "magit-mode")
(defvar magit-status-mode-map)
(defun mu-magit-kill-buffers ()
  "Restore window configuration and kill all Magit buffers."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

(add-hook 'magit-status-mode-hook
          (lambda ()
            (define-key magit-status-mode-map [(?q)] #'mu-magit-kill-buffers)))
              ;; (bind-key "q" #'mu-magit-kill-buffers magit-status-mode-map)))
