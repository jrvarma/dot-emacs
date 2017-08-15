;;  Mark files with different modification times in two dired buffers.
;;  J R Varma's modification of dired-compare-directories
(provide 'jrv-dired-compare)

(require 'dired)

(declare-function dired-dwim-target-directory "dired-aux.el")
(declare-function dired-files-attributes "dired-aux.el")
(declare-function dired-file-set-difference "dired-aux.el")

(defun dired-compare-directories-new ()
  "Mark files with different modification times in two dired buffers.
  J R Varma's modification of dired-compare-directories"
  (interactive)                                    ; added by JRVarma
  (let* ((dir1 (dired-current-directory))
         (dir2 (dired-dwim-target-directory))      ; added by JRVarma
         (predicate '(> mtime1 mtime2))           ; added by JRVarma
         (file-alist1 (dired-files-attributes dir1))
         (file-alist2 (dired-files-attributes dir2))
	 file-list1 file-list2)
    (setq file-alist1 (delq (assoc "." file-alist1) file-alist1))
    (setq file-alist1 (delq (assoc ".." file-alist1) file-alist1))
    (setq file-alist2 (delq (assoc "." file-alist2) file-alist2))
    (setq file-alist2 (delq (assoc ".." file-alist2) file-alist2))
    (setq file-list1 (mapcar
		      'cadr
                      (dired-file-set-difference
                       file-alist1 file-alist2
		       predicate))
	  file-list2 (mapcar
		      'cadr
                      (dired-file-set-difference
                       file-alist2 file-alist1
		       predicate)))
    (dired-fun-in-all-buffers
     dir1 nil
     (lambda ()
       (dired-mark-if
        (member (dired-get-filename nil t) file-list1) nil)))
    (dired-fun-in-all-buffers
     dir2 nil
     (lambda ()
       (dired-mark-if
        (member (dired-get-filename nil t) file-list2) nil)))
    (message "Marked in dir1: %s files, in dir2: %s files"
             (length file-list1)
             (length file-list2))))
