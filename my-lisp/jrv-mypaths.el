;; read shortpaths 
(provide 'jrv-mypaths)
(eval-when-compile (require 'subr-x))

;; I used to have an alist of shortpaths and associated fullpaths
;; This mapping was hard to maintain and update
;; Unix symlinks are much easier to update
;; Now the folder ~/0 contains around 70 symlinks to fullpaths
;; I want to see these not as symlinks but in their actual locations
;; Hence the code below to convert these symlinks to their realpaths
;; It is also possible to have a regular file containing the full path
;; This is useful for remote paths accessed using tramp
;; For example, the file could contain  /smb:192.168.240.1:/junk/


(defun jrv/mypaths-real-path(x)
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

(defun jrv/mypaths-get-real-path(&optional noprint)
  "Prompt for shortpath (symlink in ~/0) and return the full path of symlink target"
  (interactive)
  (let*
      ((j-paths
        (split-string (shell-command-to-string "ls -1a ~/0") "\n"))
       (path
        (jrv/mypaths-real-path
         (completing-read "Enter shortpath: " j-paths nil t))))
    (when (and
           (not (string-prefix-p "/smb:" path))
           (not (string-empty-p path))
           (file-directory-p path))
      (setq path (concat path "/")))
    (unless noprint (insert path))
    path))

;; allow use in minibuffers
(setq enable-recursive-minibuffers 1)

(declare-function read-file-name "minibuffer")
(declare-function confirm-nonexistent-file-or-buffer "files")
(defun jrv/mypaths-find-file-real-path ()
  "Prompt for shortpath (symlink in ~/0) and open the symlink target"
  (interactive)
  (let* ((path (jrv/mypaths-get-real-path t))
         (dir (file-name-directory path))
         (file (or (file-name-nondirectory path) "")))
    ;; (message "%s    %s" dir file)
    (find-file
     (read-file-name "Find file: " dir nil (confirm-nonexistent-file-or-buffer) file))))
