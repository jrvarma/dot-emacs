(provide 'jrv-git)
;;;; daily backups (with git)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THIS FILE IS PROBABLY USELESS TO ANYBODY ELSE    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun n-days-old (filename ndays)
  (< (float-time (nth 5 (file-attributes filename))) 
     (- (float-time) (* ndays 24 3600))))

(defun local-git-copy (local-file remote-file ndays)
  (when (or (not (file-exists-p local-file)) (n-days-old local-file ndays))
    (find-file remote-file)
    (write-region nil nil local-file)
    (kill-buffer (get-file-buffer remote-file))
    (message (shell-command-to-string 
              (format "bash -c 'cd %s; git commit --message=%s-%s %s'" 
                      (file-name-directory local-file) 
                      (format-time-string "%Y-%m-%d")
                      (file-name-nondirectory local-file) 
                      (file-name-nondirectory local-file))))))

