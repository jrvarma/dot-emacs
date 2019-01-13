;; read shortpaths 
(provide 'jrv-mypaths)
(eval-when-compile (require 'subr-x))

;; Earlier I used to have a dictionary of shortpaths and the associated fullpaths
;; Then I realized that this mapping is hard to maintain and update
;; It is easier to use symlinks that can be updated easily using linux commands
;; Now the folder ~/0 in my home folder contains the short paths as symlinks to fullpaths
;; There are around 60 of them currently
;; In emacs, I want to see these not as symlinks but in their actual locations
;; Hence the code below to convert these symlinks to their realpaths
;; Instead of a symlink, it is also possible to have a regular file containing the full path
;; This is useful for remote paths accessed using tramp
;; For example, the file could contain a line like /smb:192.168.240.1:/junk/


(defun real-path(x)
  "Translate shortpath (symlink in ~/0) to the full path of symlink target"
  (interactive)
  (cond ((file-symlink-p (concat "~/0/" x))
         (string-trim-right
          (shell-command-to-string
           (concat "realpath ~/0/" x))))
        ((string-empty-p x) "")
        ((file-exists-p (concat "~/0/" x))
         (string-trim-right
         (shell-command-to-string
          (concat "cat ~/0/" x))))
        (nil)))

(defun get-real-path(&optional noprint)
  "Prompt for shortpath (symlink in ~/0) and return the full path of symlink target"
  (interactive)
  (let* ((j-paths (split-string (shell-command-to-string "ls -1a ~/0") "\n"))
         (path (real-path (completing-read "Enter shortpath: " j-paths nil t))))
    (when (and
           (not (string-prefix-p "/smb:" path))
           (not (string-empty-p path))
           (file-directory-p path))
      (setq path (concat path "/")))
    (unless noprint (insert path))
    path))

;; the symlinks in ~/0 are also used to set various variables
;; that refer to commonly used files and folders (these are also used in various functions)
(mapc (lambda(x) (set (intern x) (real-path x)))
        ;; (list "jrv-htimelog" "jrv-jtimelog" "jrv-jtimelogR"
        ;;       "j-org-jrv-arch" "j-org-jrv-file"
        ;;       "h-org-jrv-arch" "h-org-jrv-file"
        ;;       "h-org-someday" "j-org-someday"
        ;;       "h-org-P" "j-org-P"))
        (list "h-time" "j-time" "j-org" "h-org"))
(set (intern "jrv-jtemp") (real-path "t"))
(set (intern "jrv-jdrive") (real-path "j"))
(set (intern "jrv-hdrive") (real-path "h"))

; allow use in minibuffers
(setq enable-recursive-minibuffers 1)
;; (global-set-key [f12] 'get-real-path)

(declare-function read-file-name "minibuffer")
(declare-function confirm-nonexistent-file-or-buffer "files")
(defun jrv-find-file-real-path ()
  (interactive)
  (let* ((path (get-real-path t))
         (dir (file-name-directory path))
         (file (or (file-name-nondirectory path) "")))
    (message "%s    %s" dir file)
    (find-file
     (read-file-name "Find file: " dir nil (confirm-nonexistent-file-or-buffer) file))))
